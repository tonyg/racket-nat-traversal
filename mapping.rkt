#lang racket/base

(require racket/match)
(require "timer.rkt")

(provide (struct-out mapping)
	 (struct-out persistent-mapping)

	 make-persistent-mapping*
	 stop-persistent-mapping!
	 current-persistent-mapping
	 refresh-persistent-mapping!)

(struct mapping (protocol internal-address internal-port external-port lifetime)
	#:prefab)
(struct persistent-mapping (thread) #:prefab)

(define (make-persistent-mapping* map-port!
				  delete-mapping!
				  external-ip-address
				  protocol
				  local-port
				  requested-port
				  #:refresh-interval [refresh-interval 3600]
				  #:on-mapping [on-mapping void])
  (define (map!)
    (map-port! protocol
	       local-port
	       requested-port
	       (* refresh-interval 2))) ;; refresh half-way to expiry, per NAT-PMP draft
  (persistent-mapping
   (thread
    (lambda ()
      (with-handlers ([(lambda (e) #t)
		       (lambda (e)
			 (on-mapping #f #f)
			 (raise e))])
	(let loop ((old-port #f) (m (map!)))
	  (if (not (equal? old-port (mapping-external-port m)))
	      (begin (on-mapping (external-ip-address) m)
		     (loop (mapping-external-port m) m))
	      (sync (handle-evt (timer-evt refresh-interval)
				(lambda (_)
				  (loop old-port (map!))))
		    (handle-evt (thread-receive-evt)
				(lambda (_)
				  (match (thread-receive)
				    ['stop
				     (delete-mapping! m)
				     (on-mapping #f #f)
				     (void)]
				    [(list 'current-mapping ch)
				     (channel-put ch m)
				     (loop old-port m)]
				    ['refresh-now!
				     (loop old-port (map!))]
				    [other
				     (log-error "Persistent mapping: bad requext: ~v" other)
				     (loop old-port m)])))))))))))

(define (stop-persistent-mapping! p)
  (with-handlers ([exn:fail? void])
    (thread-send (persistent-mapping-thread p) 'stop)))

(define (current-persistent-mapping p)
  (define ch (make-channel))
  (thread-send (persistent-mapping-thread p) (list 'current-mapping ch))
  (channel-get ch))

(define (refresh-persistent-mapping! p)
  (thread-send (persistent-mapping-thread p) 'refresh-now!))
