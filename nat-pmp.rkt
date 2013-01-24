#lang racket/base
;; Basic NAT-PMP for opening ports on routers.
;; Copyright (C) 2013 Tony Garnock-Jones <tonygarnockjones@gmail.com>.
;;
;; This file is part of racket-nat-pmp.
;;
;; racket-nat-pmp is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; racket-nat-pmp is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public
;; License along with racket-nat-pmp. If not, see
;; <http://www.gnu.org/licenses/>.

(require racket/udp)
(require racket/match)
(require racket/system)
(require racket/port)
(require racket/string)

(require (planet tonyg/bitsyntax))

(provide (struct-out mapping)
	 (struct-out persistent-mapping)

	 nat-pmp-port
	 nat-pmp-retry-count
	 nat-pmp-reply-timeout
	 nat-pmp-logger

	 gateway-ip-address
	 nat-pmp-transaction!
	 external-ip-address
	 map-port!
	 unmap-port!
	 unmap-all-ports!
	 refresh-mapping!
	 delete-mapping!

	 start-nat-change-listener!
	 stop-nat-change-listener!

	 make-persistent-mapping
	 stop-persistent-mapping!
	 current-persistent-mapping
	 refresh-persistent-mapping!)

(struct mapping (protocol internal-port external-port lifetime) #:prefab)
(struct persistent-mapping (thread) #:prefab)

(define nat-pmp-port (make-parameter 5351))
(define nat-pmp-retry-count (make-parameter 3))
(define nat-pmp-reply-timeout (make-parameter 1))
(define nat-pmp-logger (make-parameter (make-logger #f #f)))

(define (gateway-ip-address)
  (string-trim
   (with-output-to-string
     (lambda () (system "netstat -rn | grep '^default' | awk '{print $2}'")))))

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

(define (timer-evt delay-seconds)
  (alarm-evt (+ (current-inexact-milliseconds) (* delay-seconds 1000.0))))

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

(define (call-with-udp-socket interface port f)
  (define s (udp-open-socket))
  (udp-bind! s interface port)
  (call-with-values (lambda () (f s))
    (lambda vs
      (udp-close s)
      (apply values vs))))

(define (parse-address-response packet)
  (bit-string-case packet
    ([  (= 0) (= 128) (result-code :: bytes 2)
	(mapping-table-age :: bytes 4)
	a b c d  ]
     (check-result-code! result-code)
     (format "~a.~a.~a.~a" a b c d))))

(define (external-ip-address)
  (call-with-udp-socket #f 0
   (lambda (s)
     (parse-address-response (nat-pmp-transaction! s (bytes 0 0))))))

(define (map-port! protocol local-port requested-port lifetime-seconds)
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
		 local-port
		 mapped-port
		 mapped-lifetime))))))

(define (unmap-port! protocol local-port)
  (map-port! protocol local-port 0 1)
  (void))

(define (unmap-all-ports! protocol)
  (unmap-port! protocol 0))

(define (refresh-mapping! m)
  (match-define (mapping p l r t) m)
  (map-port! p l r t))

(define (delete-mapping! m)
  (match-define (mapping p l r t) m)
  (unmap-port! p l))

(define (start-nat-change-listener!)
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

(define (stop-nat-change-listener! t)
  (thread-send t 'stop))

(define (make-persistent-mapping protocol
				 local-port
				 requested-port
				 #:refresh-interval [refresh-interval 7200]
				 #:on-mapping [on-mapping void])
  (define (map!)
    (map-port! protocol
	       local-port
	       requested-port
	       (+ refresh-interval 5))) ;; a few seconds' grace
  (persistent-mapping
   (thread
    (lambda ()
      (let loop ((old-port #f) (m (map!)))
	(if (not (equal? old-port (mapping-external-port m)))
	    (begin (on-mapping m)
		   (loop (mapping-external-port m) m))
	    (sync (handle-evt (timer-evt refresh-interval)
			      (lambda (_)
				(loop old-port (map!))))
		  (handle-evt (thread-receive-evt)
			      (lambda (_)
				(match (thread-receive)
				  ['stop
				   (delete-mapping! m)
				   (void)]
				  [(list 'current-mapping ch)
				   (channel-put ch m)
				   (loop old-port m)]
				  ['refresh-now!
				   (loop old-port (map!))]
				  [other
				   (log-error "Persistent mapping: bad requext: ~v" other)
				   (loop old-port m)]))))))))))

(define (stop-persistent-mapping! p)
  (thread-send (persistent-mapping-thread p) 'stop))

(define (current-persistent-mapping p)
  (define ch (make-channel))
  (thread-send (persistent-mapping-thread p) (list 'current-mapping ch))
  (channel-get ch))

(define (refresh-persistent-mapping! p)
  (thread-send (persistent-mapping-thread p) 'refresh-now!))
