#lang racket

(require "../high-level.rkt")

(define port (match (current-command-line-arguments)
               [(vector s) (string->number s)]
               [_ 5999]))

(define (handle-mapping-change assignments)
  (pretty-print `(assignments . ,(set->list assignments))))

(define s (udp-open-socket))
(define mapping-manager (udp-bind!/public s #f port #:on-mapping-change handle-mapping-change))

(define buffer (make-bytes 65536))
(let loop ()
  (sync (handle-evt (udp-receive!-evt s buffer)
		    (match-lambda
		     [(list packet-length remote-address remote-port)
		      (match (string-trim (bytes->string/utf-8 (subbytes buffer 0 packet-length)))
			["quit"
			 (printf "Told to quit.\n")
			 (mapping-change-listener-stop! mapping-manager)
			 (sleep 0.5)
			 (udp-close s)]
			[packet
			 (udp-send-to s remote-address remote-port
				      (string->bytes/utf-8
				       (format "You said: ~a\n" packet)))
			 (loop)])]))))
