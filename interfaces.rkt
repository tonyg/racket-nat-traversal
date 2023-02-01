#lang racket/base

(require racket/match)
(require racket/port)
(require racket/system)
(require racket/string)
(require racket/list)
(require racket/function)

(provide gateway-ip-address
	 interface-ip-addresses
	 best-interface-ip-address
         ip-address?
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

(define (interface-ip-addresses)
  (map
   (lambda (pieces)
     (car (car (filter-map (lambda (s) (regexp-match #px"\\d+\\.\\d+\\.\\d+\\.\\d+" s)) pieces))))
   (filter (lambda (r) (and (pair? r) (string-ci=? (car r) "inet")))
	   (map string-split
		(string-split (with-output-to-string (lambda () (system "ifconfig"))) "\n")))))

(define (ip-address? x)
  (and (string? x)
       (regexp-match? #px"^\\d+\\.\\d+\\.\\d+\\.\\d+$" x)))

(define (wildcard-ip-address? x)
  (equal? x "0.0.0.0"))

(define (localhost-ip-address? x)
  (and (ip-address? x) (string-prefix? x "127.")))

(define (private-ip-address? x)
  (and (ip-address? x)
       (match (map string->number (string-split x "."))
         [(list 10 _ _ _) #t]
         [(list 172 n _ _) (and (>= n 16) (< n 32))]
         [(list 192 168 _ _) #t]
         [(list 100 n _ _) (and (>= n 64) (< n 128))] ;; CGNAT, Carrier Grade NAT, RFC 6598
         [_ #f])))

(define (binary-byte s)
  (local-require (only-in racket/format ~r))
  (~r (string->number s) #:base 2 #:min-width 8 #:pad-string "0"))

(define (common-prefix ip1 ip2)
  (for/or [(i (in-naturals))
           (c1 (in-string (string-append* (map binary-byte (string-split ip1 ".")))))
           (c2 (in-string (string-append* (map binary-byte (string-split ip2 ".")))))]
    (and (not (eqv? c1 c2)) i)))

(define (best-interface-ip-address #:peer-ip [peer-ip #f])
  (define addresses (interface-ip-addresses))
  (if peer-ip
      (car (sort addresses
                 (lambda (a b) (> (common-prefix peer-ip a) (common-prefix peer-ip b)))))
      (let ()
        (define local-addresses (filter localhost-ip-address? addresses))
        (define nonlocal-addresses (filter (negate localhost-ip-address?) addresses))
        (define public-addresses (filter (negate private-ip-address?) nonlocal-addresses))
        (define private-addresses (filter private-ip-address? nonlocal-addresses))

        (cond
          [(pair? public-addresses) (car public-addresses)]
          [(pair? private-addresses) (car private-addresses)]
          [(pair? local-addresses) (car local-addresses)]
          [else "127.0.0.1"]))))
