#lang racket/base
;; High-level interface to NAT traversal functionality.

(require racket/udp)
(require racket/tcp)

(require racket/set)
(require racket/match)

(require "nat-pmp.rkt")
(require "upnp-ip-gateway.rkt")
(require "interfaces.rkt")
(require "mapping.rkt")

(provide udp-bind!/public
	 tcp-listen/public

         (rename-out [make-mapping-change-listener mapping-change-listener])
         (rename-out [mapping-change-listener <mapping-change-listener>])
         mapping-change-listener?
         mapping-change-listener-thread
	 mapping-change-listener-current-mappings
	 mapping-change-listener-stop!

	 (struct-out port-assignment))

(struct mapping-change-listener (thread) #:prefab)
(struct port-assignment (protocol address port nat-traversal-technique) #:prefab)

(define (mapping-change-listener-current-mappings mcl)
  (define ch (make-channel))
  (thread-send (mapping-change-listener-thread mcl) (list 'current-mappings ch))
  (channel-get ch))

(define (mapping-change-listener-stop! mcl)
  (with-handlers ([exn:fail? void])
    (thread-send (mapping-change-listener-thread mcl) 'stop)))

(define (udp-bind!/public socket hostname-string port-no
			  #:on-mapping-change [on-mapping-change void])
  (udp-bind! socket hostname-string port-no)
  (define-values (local-address local-port remote-address remote-port)
    (udp-addresses socket #t))
  (make-mapping-change-listener 'udp local-address local-port on-mapping-change))

(define (tcp-listen/public port-no [max-allow-wait 4] [reuse? #f] [hostname #f]
			   #:on-mapping-change [on-mapping-change void])
  (define listener (tcp-listen port-no max-allow-wait reuse? hostname))
  (define-values (local-address local-port remote-address remote-port)
    (tcp-addresses listener #t))
  (values listener
	  (make-mapping-change-listener 'tcp local-address local-port on-mapping-change)))

(define (make-mapping-change-listener protocol initial-local-address local-port on-mapping-change)
  (start-upnp-gateway-scanner!)
  (mapping-change-listener
   (thread
    (lambda ()
      (with-handlers ([(lambda (e) #t)
		       (lambda (e)
			 (on-mapping-change (set))
			 (raise e))])
	(define mapping-thread (current-thread))
	(define ((handle-change technique) external-ip m)
	  (with-handlers ([exn:fail? void])
	    (thread-send mapping-thread (list 'update technique external-ip m))))
	(define nat-pmp-mapping (nat-pmp-make-persistent-mapping protocol local-port local-port
								 #:on-mapping
								 (handle-change 'nat-pmp)))
	(define upnp-mapping (upnp-make-persistent-mapping protocol local-port local-port
							   #:on-mapping
							   (handle-change 'upnp)))
        (define initial-addresses (compute-initial-addresses protocol
							     initial-local-address
							     local-port))

	(let loop ((extra-addresses (hash)))
	  (define current-state (build-current-state initial-addresses extra-addresses))
	  (on-mapping-change current-state)
	  (let loop/nochange ()
	    (sync (handle-evt (thread-receive-evt)
			      (lambda (_)
				(match (thread-receive)
				  [(list 'update technique _ #f)
				   (if (hash-has-key? extra-addresses technique)
				       (loop (hash-remove extra-addresses technique))
				       (loop/nochange))]
				  [(list 'update technique external-ip m)
				   (if (wildcard-ip-address? external-ip)
				       (loop/nochange)
				       (loop (hash-set extra-addresses technique
						       (mapping->assignment m
									    external-ip
									    technique))))]
				  [(list 'current-mappings ch)
				   (channel-put ch current-state)
				   (loop/nochange)]
				  ['stop
				   (stop-persistent-mapping! nat-pmp-mapping)
				   (stop-persistent-mapping! upnp-mapping)
				   (on-mapping-change (set))])))))))))))

(define (compute-initial-addresses protocol initial-local-address local-port)
  (cond
   [(wildcard-ip-address? initial-local-address)
    ;; List all the local interface addresses, then.
    (map (lambda (a) (port-assignment protocol a local-port #f))
	 (interface-ip-addresses))]
   [else
    ;; Just a single interface.
    (list (port-assignment protocol initial-local-address local-port #f))]))

(define (mapping->assignment m external-ip technique)
  (port-assignment (mapping-protocol m)
		   external-ip
		   (mapping-external-port m)
		   technique))

(define (build-current-state initial-addresses extra-addresses)
  (list->set (append (hash-values extra-addresses) initial-addresses)))
