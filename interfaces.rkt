#lang racket/base

(require racket/match)
(require racket/port)
(require racket/system)
(require racket/string)
(require racket/list)
(require racket/function)
(require net/ip)
(require racket/pretty)

(provide gateway-ip-address
	 interface-ip-addresses
	 best-interface-ip-address
	 wildcard-ip-address?
	 localhost-ip-address?
	 private-ip-address?)

(define (gateway-ip-address)
  (define r
    (findf
     (lambda (r) (and (pair? r) (member (car r) '("default" "0.0.0.0"))))
     (map string-split
	  (string-split (with-output-to-string (lambda () (system "netstat -rn"))) "\n"))))
  (when (not r)
    (error 'gateway-ip-address "Cannot determine gateway IP address"))
  (cadr r))

(define ipv4-regex #px"\\d+.\\d+.\\d+.\\d+")

(define (ipv6? x)
  (with-handlers ([exn:fail:contract? (λ (ex) #f)]) (string->ipv6-address x)))

(define (get-ipv6? x)
  (if (ipv6? x) (list x) #f))

(define (interface-ip-addresses)
  (map
   (lambda (pieces)
     (car (car (filter-map (lambda (s) (or (get-ipv6? s) (regexp-match ipv4-regex s))) pieces))))
   (filter (lambda (r) (and (pair? r) (or (string-ci=? (car r) "inet") (string-ci=? (car r) "inet6"))))
	   (map string-split
		(string-split (with-output-to-string (lambda () (system "ifconfig"))) "\n")))))

(define (local-ipv6? x)
  (if (ipv6? x) (let* ([split-ip (string-split x ":")]
                       [start (car split-ip)])
                   (or (equal? start "fe80")
                       (equal? start "fc00")
                       (equal? x "::1")))
    #f))


(define (wildcard-ip-address? x)
  (equal? x "0.0.0.0"))

(define (localhost-ip-address? x)
  (string-prefix? x "127."))

(define (private-ip-address? x)
  (match (map string->number (string-split x "."))
    [(list 10 _ _ _) #t]
    [(list 172 n _ _) (and (>= n 16) (< n 32))]
    [(list 192 168 _ _) #t]
    [_ #f]))

(define (best-interface-ip-address)
  (define addresses (interface-ip-addresses))

  (define ipv6-addresses (filter ipv6? addresses))
  (define public-ipv6-addresses (filter (negate local-ipv6?) ipv6-addresses))
  (define local-ipv6-addresses (filter local-ipv6? ipv6-addresses))
  (define local-addresses (filter localhost-ip-address? addresses))
  (define nonlocal-addresses (filter (negate localhost-ip-address?) addresses))
  (define public-addresses (filter (negate private-ip-address?) nonlocal-addresses))
  (define private-addresses (filter private-ip-address? nonlocal-addresses))

  (cond
   [(pair? public-ipv6-addresses) (car public-ipv6-addresses)]
   [(pair? public-addresses) (car public-addresses)]
   [(pair? local-ipv6-addresses) (car local-ipv6-addresses)]
   [(pair? private-addresses) (car private-addresses)]
   [(pair? local-addresses) (car local-addresses)]
   [else "127.0.0.1"]))
