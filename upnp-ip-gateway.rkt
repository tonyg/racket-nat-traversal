#lang racket/base
;; UPNP WANIPConnection service client.
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

(require "upnp.rkt")
(require "interfaces.rkt")
(require "mapping.rkt")
(require "timer.rkt")

(require racket/match)

(provide current-ip-gateway
	 gateway-rescan-interval
	 start-upnp-gateway-scanner!
	 stop-upnp-gateway-scanner!

	 upnp-external-ip-address

	 upnp-query-port-mapping
	 upnp-map-port!*
	 upnp-map-port!
	 upnp-refresh-mapping!
	 upnp-delete-mapping!

	 upnp-make-persistent-mapping
	 )

(define *current-ip-gateway* #f)

(define (current-ip-gateway)
  (let loop ()
    (when (not *current-ip-gateway*)
      (when *gateway-scanner*
	(sleep 0.1) ;; eww
	(loop))))
  *current-ip-gateway*)

(define gateway-rescan-interval (make-parameter 300)) ;; five minutes

(define *gateway-scanner* #f)

(define (log-crashed-gateway e)
  (log-error "UPnP gateway scanner died with exn: ~a" (exn-message e)))

(define (start-upnp-gateway-scanner!)
  (when (not *gateway-scanner*)
    (set! *gateway-scanner* (thread (lambda ()
				      (with-handlers ([exn? log-crashed-gateway])
					(scan-for-gateways))
				      (set! *gateway-scanner* #f))))))

(define (stop-upnp-gateway-scanner!)
  (when *gateway-scanner*
    (thread-send *gateway-scanner* 'stop)
    (set! *gateway-scanner* #f)))

(define (scan-for-gateways)
  (let/ec ignore-remainder-of-scan
    (for ([gw (in-upnp-services)]
	  #:when (or (upnp-service-type=? gw "urn:schemas-upnp-org:service:WANIPConnection:1")
                     (upnp-service-type=? gw "urn:schemas-upnp-org:service:WANPPPConnection:1")))
      ;; (pretty-print `(found-gateway gw))
      (set! *current-ip-gateway* gw)
      (ignore-remainder-of-scan)))
  (when (not *current-ip-gateway*)
    (log-info "upnp: scan-for-gateways: discovery timeout without finding anything. Retry in ~a seconds."
	      (gateway-rescan-interval)))
  (sync (handle-evt (timer-evt (* (gateway-rescan-interval) 1000))
		    (lambda (_) (scan-for-gateways)))
	(handle-evt (thread-receive-evt)
		    (lambda (_)
		      (match (thread-receive)
			['stop (void)])))))

(define (upnp-external-ip-address #:gateway [gw (current-ip-gateway)])
  (hash-ref (gw "GetExternalIPAddress") "NewExternalIPAddress"))

(define (protocol->string protocol)
  (case protocol
    ((udp) "UDP")
    ((tcp) "TCP")))

(define (upnp-query-port-mapping protocol requested-port
				 #:gateway [gw (current-ip-gateway)])
  (with-handlers ([(exn:fail:upnp?/code 714) (lambda (e) #f)]) ;; NoSuchEntryInArray
    (define result (gw "GetSpecificPortMappingEntry"
		       "" ;; NewRemoteHost
		       (number->string requested-port)
		       (protocol->string protocol)))
    (mapping protocol
	     (hash-ref result "NewInternalClient")
	     (string->number (hash-ref result "NewInternalPort"))
	     requested-port
	     (string->number (hash-ref result "NewLeaseDuration")))))

(define (upnp-map-port!* protocol local-port requested-port lifetime-seconds
			 #:gateway [gw (current-ip-gateway)]
			 #:local-address [local-address (best-interface-ip-address)]
			 #:description [description ""])
  (when (not (zero? lifetime-seconds))
    (log-warning "upnp-map-port!*: Non-zero (non-infinite) lifetime-seconds is not recommended"))
  ;; "AddPortMapping" raises an exception if it fails. This API design is nuts.
  (gw "AddPortMapping"
      "" ;; NewRemoteHost
      (number->string requested-port)
      (protocol->string protocol)
      (number->string local-port)
      local-address
      "1"
      description
      "0" ;; we IGNORE lifetime-seconds here. According to the NAT-PMP
	  ;; draft, a significant fraction of UPnP implementations are
	  ;; buggy enough that requesting non-zero (non-infinite)
	  ;; leases can cause severe problems, e.g. crashing etc.
      )
  (or (upnp-query-port-mapping protocol requested-port #:gateway gw)
      (error 'upnp-map-port!*
	     "Failed to map requested port ~a to local ~a port ~a"
	     requested-port
	     protocol
	     local-port)))

;; Like upnp-map-port!*, but behaves more like NAT-PMP in cases where
;; there's an existing mapping at the requested port. A requested-port
;; of zero indicates a high-numbered ephemeral port should be chosen;
;; any other value indicates a preferred port, which may not be the
;; one actually granted by the server.
(define (upnp-map-port! protocol local-port requested-port lifetime-seconds
			#:gateway [gw (current-ip-gateway)]
			#:local-address [local-address (best-interface-ip-address)]
			#:description [description ""])
  ;; See http://en.wikipedia.org/wiki/Ephemeral_port
  (define (random-port) (+ 1025 (random (- 65536 1025))))
  (let retry ((retry-count 0)
	      (requested-port (if (zero? requested-port)
				  (+ 32768 (random (- 61000 32768)))
				  requested-port)))
    (when (> retry-count 10)
      (error 'upnp-map-port! "Could not find a free port after 10 tries"))
    (match (upnp-query-port-mapping protocol requested-port #:gateway gw)
      [#f ;; it's free! try it.
       (log-info "upnp-map-port!: ~a/~a free, claiming" protocol requested-port)
       (with-handlers ([(exn:fail:upnp?/code 718) ;; ConflictInMappingEntry
			(lambda (e) ;; it got taken...?
			  (log-info "upnp-map-port!: ~a/~a conflicted, retrying"
				    protocol requested-port)
			  (retry (+ retry-count 1) (random-port)))])
	 (upnp-map-port!* protocol local-port requested-port lifetime-seconds
			  #:gateway gw
			  #:local-address local-address
			  #:description description))]
      [(and (mapping (== protocol) (== local-address) (== local-port) (== requested-port) lifetime)
	    existing-mapping)
       ;; It's already set to what we asked for! No action required.
       (log-info "upnp-map-port!: ~a/~a already set up" protocol requested-port)
       existing-mapping]
      [(? mapping?) ;; It's mapped to someone else. Try a different port number.
       (log-info "upnp-map-port!: ~a/~a assigned differently, retrying" protocol requested-port)
       (retry (+ retry-count 1) (random-port))])))

(define (upnp-refresh-mapping! m)
  (match-define (mapping p _ l r t) m)
  (upnp-map-port!* p l r t))

(define (upnp-delete-mapping! m
			      #:gateway [gw (current-ip-gateway)])
  (match-define (mapping p _ _ r _) m)
  (gw "DeletePortMapping"
      "" ;; NewRemoteHost
      (number->string r)
      (protocol->string p))
  (void))

(define (upnp-make-persistent-mapping protocol
				      local-port
				      requested-port
				      #:refresh-interval [refresh-interval 3600]
				      #:on-mapping [on-mapping void]
				      #:gateway-proc [gw-proc current-ip-gateway]
				      #:local-address [local-address (best-interface-ip-address)]
				      #:description [description ""])
  (make-persistent-mapping* (lambda (p l r t)
			      (upnp-map-port! p l r
					      0 ;; unilaterally decide to ignore lifetime
					      #:gateway (gw-proc)
					      #:local-address local-address
					      #:description description))
			    (lambda (m) (upnp-delete-mapping! m #:gateway (gw-proc)))
			    (lambda () (upnp-external-ip-address #:gateway (gw-proc)))
			    protocol
			    local-port
			    requested-port
			    #:refresh-interval refresh-interval
			    #:on-mapping on-mapping))

;; (require racket/pretty)
;; (require racket/trace)
;; (trace upnp-map-port!*)
;; (trace upnp-delete-mapping!)
