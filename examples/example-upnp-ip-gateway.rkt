#lang racket/base

(require racket/pretty)

(require "../upnp-ip-gateway.rkt")

;;---------------------------------------------------------------------------

(start-upnp-gateway-scanner!)
(upnp-external-ip-address)

(when (upnp-query-port-mapping 'udp 2000)
  (upnp-delete-mapping! (upnp-query-port-mapping 'udp 2000)))

(define m1 (upnp-map-port! 'udp 5999 2000 30))
(pretty-print m1)
(pretty-print (upnp-query-port-mapping 'udp 2000))

(define m2 (upnp-map-port! 'udp 5998 2000 30 #:local-address "192.168.99.99"))
(pretty-print m2)
(upnp-delete-mapping! m2)

(define m3 (upnp-map-port! 'udp 5998 2000 30))
(pretty-print m3)
(upnp-delete-mapping! m3)

(upnp-delete-mapping! m1)
(pretty-print (upnp-query-port-mapping 'udp 2000))
