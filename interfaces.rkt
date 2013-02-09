#lang racket/base

(require racket/match)
(require racket/port)
(require racket/system)
(require racket/string)
(require racket/list)
(require (only-in srfi/13 string-prefix?))

(provide gateway-ip-address
	 interface-ip-addresses
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
     (car (car (filter-map (lambda (s) (regexp-match #px"\\d+.\\d+.\\d+.\\d+" s)) pieces))))
   (filter (lambda (r) (and (pair? r) (string-ci=? (car r) "inet")))
	   (map string-split
		(string-split (with-output-to-string (lambda () (system "ifconfig"))) "\n")))))

(define (localhost-ip-address? x)
  (string-prefix? "127." x))

(define (private-ip-address? x)
  (match (map string->number (string-split x "."))
    [(list 10 _ _ _) #t]
    [(list 172 n _ _) (and (>= n 16) (< n 32))]
    [(list 192 168 _ _) #t]
    [_ #f]))
