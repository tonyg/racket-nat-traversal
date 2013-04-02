#lang racket

(require "../high-level.rkt")

(define control-channel (make-channel))

(define (connection-handler cin cout)
  (fprintf cout "Welcome to the example server!\n")
  (flush-output cout)
  (let loop ()
    (match (read-line cin)
      [(? eof-object?)
       (fprintf cout "Bye!\n")
       (flush-output cout)
       (void)]
      ["quit" (channel-put control-channel 'quit)]
      [exp
       (fprintf cout "You said: ~a\n" exp)
       (flush-output cout)
       (loop)])))

(define (handle-mapping-change assignments)
  (pretty-print `(assignments . ,(set->list assignments))))

(let-values (((listener mapping-manager)
	      (tcp-listen/public 5999 4 #t #:on-mapping-change handle-mapping-change)))
  (let loop ()
    (sync (handle-evt (tcp-accept-evt listener)
		      (match-lambda
		       [(list cin cout)
			(thread (lambda () (connection-handler cin cout)))
			(loop)]))
	  (handle-evt control-channel
		      (match-lambda
		       ['quit
			(printf "Told to quit.\n")
			(mapping-change-listener-stop! mapping-manager)
			(sleep 0.5)
			(tcp-close listener)])))))
