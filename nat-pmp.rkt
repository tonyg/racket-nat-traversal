#lang racket/base
;; Basic NAT-PMP for opening ports on routers.
;; Copyright (C) 2013 Tony Garnock-Jones <tonygarnockjones@gmail.com>.
;;
;; This file is part of racket-nat-traversal.
;;
;; racket-nat-traversal is free software: you can redistribute it
;; and/or modify it under the terms of the GNU Affero General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;;
;; racket-nat-traversal is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public
;; License along with racket-nat-traversal. If not, see
;; <http://www.gnu.org/licenses/>.

(require racket/udp)
(require racket/match)

(require bitsyntax)

(require "interfaces.rkt")
(require "mapping.rkt")
(require "timer.rkt")

(provide nat-pmp-port
	 nat-pmp-retry-count
	 nat-pmp-reply-timeout
	 nat-pmp-logger

	 nat-pmp-transaction!
	 nat-pmp-external-ip-address
	 nat-pmp-map-port!
	 nat-pmp-unmap-all-ports!
	 nat-pmp-refresh-mapping!
	 nat-pmp-delete-mapping!

	 start-nat-pmp-change-listener!
	 stop-nat-pmp-change-listener!

	 nat-pmp-make-persistent-mapping)

(define nat-pmp-port (make-parameter 5351))
(define nat-pmp-retry-count (make-parameter 3))
(define nat-pmp-reply-timeout (make-parameter 1))
(define nat-pmp-logger (make-parameter (make-logger #f #f)))

(define (check-result-code! code)
  (if (zero? code)
      #t
      (error 'nat-pmp (case code
			((1) "Unsupported version")
			((2) "Not authorized")
			((3) "Network failure")
			((4) "Out of resources")
			((5) "Unsupported opcode")
			(else (format "Unknown result code ~v" code))))))

(define (nat-pmp-transaction! s request)
  (let loop ((remaining-attempts (nat-pmp-retry-count)))
    (if (zero? remaining-attempts)
	(error 'nat-pmp "No reply")
	(let ((buffer (make-bytes 128)))
	  (udp-send-to s (gateway-ip-address) (nat-pmp-port) (bit-string->bytes request))
	  (sync (handle-evt (udp-receive!-evt s buffer)
			    (match-lambda
			     [(list count _ _)
			      (subbytes buffer 0 count)]))
		(handle-evt (timer-evt (nat-pmp-reply-timeout))
			    (lambda (_)
			      (loop (- remaining-attempts 1)))))))))

(define (udp-close/no-error s)
  (with-handlers ([exn:fail? void]) (udp-close s)))

(define (call-with-udp-socket interface port f)
  (define s (udp-open-socket))
  (udp-bind! s interface port)
  (with-handlers ([(lambda (e) #t) (lambda (e)
				     (udp-close/no-error s)
				     (raise e))])
    (call-with-values (lambda () (f s))
      (lambda vs
	(udp-close/no-error s)
	(apply values vs)))))

(define (parse-address-response packet)
  (bit-string-case packet
    ([  (= 0) (= 128) (result-code :: bytes 2)
	(mapping-table-age :: bytes 4)
	a b c d  ]
     (check-result-code! result-code)
     (format "~a.~a.~a.~a" a b c d))))

(define (nat-pmp-external-ip-address)
  (call-with-udp-socket #f 0
   (lambda (s)
     (parse-address-response (nat-pmp-transaction! s (bytes 0 0))))))

(define (nat-pmp-map-port! protocol local-port requested-port lifetime-seconds)
  (call-with-udp-socket #f 0
   (lambda (s)
     (bit-string-case (nat-pmp-transaction! s
		       (bit-string 0 ;; version
				   (case protocol
				     ((udp) 1)
				     ((tcp) 2)
				     (else (error 'nat-pmp "Invalid protocol ~v" protocol)))
				   0 0 ;; reserved
				   [local-port :: bytes 2]
				   [requested-port :: bytes 2]
				   [lifetime-seconds :: bytes 4]))
       ([  (= 0) opcode (result-code :: bytes 2)
	   (mapping-table-age :: bytes 4)
	   (= local-port :: bytes 2) (mapped-port :: bytes 2)
	   (mapped-lifetime :: bytes 4)  ]
	(match* (protocol opcode)
	  [('udp 129) #t]
	  [('tcp 130) #t]
	  [(_ _)
	   (error 'nat-pmp "Unexpected reply opcode ~a for protocol ~a" opcode protocol)])
	(check-result-code! result-code)
	(mapping protocol
		 #f
		 local-port
		 mapped-port
		 mapped-lifetime))))))

(define (unmap-port! protocol local-port)
  (nat-pmp-map-port! protocol local-port 0 1)
  (void))

(define (nat-pmp-unmap-all-ports! protocol)
  (unmap-port! protocol 0))

(define (nat-pmp-refresh-mapping! m)
  (match-define (mapping p _ l r t) m)
  (nat-pmp-map-port! p l r t))

(define (nat-pmp-delete-mapping! m)
  (match-define (mapping p _ l r t) m)
  (unmap-port! p l))

(define (start-nat-pmp-change-listener!)
  (thread
   (lambda ()
     (call-with-udp-socket "224.0.0.1" 5350
      (lambda (s)
	(let loop ()
	  (define buffer (make-bytes 128))
	  (sync (handle-evt (udp-receive!-evt s buffer)
			    (match-lambda
			     [(list count remote-ip _)
			      (with-handlers ((exn:fail? (lambda (e) 'ignore)))
				(define new-ip
				  (parse-address-response (subbytes buffer count)))
				(when (equal? remote-ip (gateway-ip-address))
				  (log-message (nat-pmp-logger)
					       'info
					       (format "IP address changed: ~a" new-ip)
					       new-ip)))
			      (loop)]))
		(handle-evt (thread-receive-evt)
			    (lambda (_)
			      (match (thread-receive)
				['stop
				 (void)]
				[other
				 (log-error "NAT change listener: bad request ~v" other)
				 (loop)]))))))))))

(define (stop-nat-pmp-change-listener! t)
  (thread-send t 'stop))

(define (nat-pmp-make-persistent-mapping protocol
					 local-port
					 requested-port
					 #:refresh-interval [refresh-interval 3600]
					 #:on-mapping [on-mapping void])
  (make-persistent-mapping* nat-pmp-map-port!
			    nat-pmp-delete-mapping!
			    nat-pmp-external-ip-address
			    protocol
			    local-port
			    requested-port
			    #:refresh-interval refresh-interval
			    #:on-mapping on-mapping))
