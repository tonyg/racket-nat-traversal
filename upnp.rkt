#lang racket/base
;; Basic UPNP service library.
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

(require racket/udp)

(require racket/set)
(require racket/match)
(require (only-in racket/list filter-map append-map))
(require racket/function)
(require racket/generator)
(require (only-in racket/port port->string))
(require (only-in racket/string string-trim))

(require xml)
(require xml/path)
(require net/url)
(require net/dns)

(require (only-in web-server/http/request read-headers))
(require web-server/http/request-structs)

(require (only-in "interfaces.rkt" ip-address?))

(provide default-scan-time

	 (struct-out exn:fail:upnp)
	 exn:fail:upnp?/code

	 (struct-out upnp-service)
	 (struct-out upnp-service-action)

	 in-upnp-services
	 upnp-service-type=?)

;;---------------------------------------------------------------------------

(define default-scan-time (make-parameter 10))

(struct exn:fail:upnp exn:fail (code description) #:transparent)

(struct upnp-discovery-response (packet host port) #:prefab)
(struct upnp-service (type control-url event-url scpd-url ip) #:prefab)
(struct upnp-service-action (name args results) #:prefab)

(define (exn:fail:upnp?/code code)
  (lambda (e)
    (and (exn:fail:upnp? e)
	 (equal? (exn:fail:upnp-code e) code))))

;;---------------------------------------------------------------------------
;; SSDP: Discovery of services

(define ssdp-address "239.255.255.250")
(define ssdp-port 1900)

(define (in-upnp-discovery #:scan-time [scan-time (default-scan-time)])
  (define s (udp-open-socket))
  (udp-bind! s #f 0)
  (issue-search-query! s)
  (define deadline (alarm-evt (+ (current-inexact-milliseconds) (* scan-time 1000))))
  (define buffer (make-bytes 8192))
  (in-generator
   (let loop ()
     (sync (handle-evt deadline
		       (lambda (_)
			 (udp-close s)))
	   (handle-evt (udp-receive!-evt s buffer)
		       (match-lambda
			[(list count remote-name remote-port)
			 (define-values (code headers)
			   (with-handlers ((exn:fail? (lambda (e)
							(write e) (newline) (flush-output)
							(values 500 '()))))
			     (parse-packet (open-input-bytes (subbytes buffer 0 count)))))
			 (when (< 199 code 300) ;; 2xx
			   (define r (upnp-discovery-response headers
							      remote-name
							      remote-port))
			   ;; (pretty-print r) (newline) (flush-output)
			   (yield r))
			 (loop)]))))))

(define (issue-search-query! s)
  (define q (string-append "M-SEARCH * HTTP/1.1\r\n"
			   "HOST: " ssdp-address ":" (number->string ssdp-port) "\r\n"
			   "MAN: \"ssdp:discover\"\r\n"
			   "ST: ssdp:all\r\n"
			   "MX: 3\r\n"
			   "\r\n"))
  ;; (printf "Issuing search query:\n~a\n" q)
  (udp-send-to s ssdp-address ssdp-port (string->bytes/utf-8 q)))

(define (parse-packet i)
  (define (next-line) (read-line i 'return-linefeed))
  (match-define (list _ http-version code-str message)
    (regexp-match #rx"([^ ]+) ([^ ]+) (.*)" (next-line)))
  (define headers (read-headers i))
  (values (string->number code-str)
	  headers))

(define (get-header name r)
  (findf (lambda (h) (string-ci=? (bytes->string/latin-1 (header-field h)) name))
	 (upnp-discovery-response-packet r)))

;;---------------------------------------------------------------------------
;; SOAP: Invoking services

;; Strip out useless whitespace from an xexpr
(define (clean-xexpr x)
  (match x
    [(? string?) (let ((s (string-trim x)))
                   (if (equal? s "")
                       '()
                       (list s)))]
    [(list* (? symbol? tag) rest)
     (list (list* tag (append-map clean-xexpr rest)))]
    [_ (list x)])) ;; attribute lists

(define (->xexpr port)
  (with-handlers [(exn:fail? (lambda (_e) #f))]
    (car (clean-xexpr (xml->xexpr (document-element (read-xml port)))))))

(define (http-get->xml u)
  (->xexpr (get-pure-port u)))

(define (http-post->xml u headers xml-to-post)
  (define xml-bytes (string->bytes/utf-8 (string-append
					  "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
					  (xexpr->string xml-to-post))))
  (->xexpr (post-pure-port u xml-bytes headers)))

(define (element-is? tag e)
  (and (pair? e)
       (eq? (car e) tag)))

(define (scpd->actions scpd)
  (for/list ([a (se-path*/list '(actionList) scpd)]
	     #:when (element-is? 'action a))
    (define args (for/list ([arg (se-path*/list '(argumentList) a)]
			    #:when (element-is? 'argument arg))
		   arg))
    (define ((arg-has-direction? direction) arg)
      (and (equal? (se-path* '(direction) arg) direction)
	   (se-path* '(name) arg)))
    (upnp-service-action
     (se-path* '(name) a)
     (filter-map (arg-has-direction? "in") args)
     (filter-map (arg-has-direction? "out") args))))

(define (raise-upnp-error code desc)
  (raise (exn:fail:upnp (format "UPnPError: ~a ~a" code desc)
			(current-continuation-marks)
			code
			desc)))

(define (service-wrapper s)
  (define scpd (http-get->xml (upnp-service-scpd-url s)))
  (and scpd
       (let ()
         (define actions (for/hash ([a (scpd->actions scpd)])
                           (values (upnp-service-action-name a) a)))
         ;; (println s)
         ;; (pretty-print actions)
         (define (upnp-service-dispatcher op . args)
           (match op
             ['descriptor
              s]
             ['actions
              actions]
             [(? string?)
              (define a (hash-ref actions op (lambda () #f)))
              (when (not a)
                (error 'service-wrapper "No such action: ~v" op))
              (define action-tag (string->symbol (string-append "u:" op)))
              (define response-tag (string->symbol (string-append "u:" op "Response")))
              (when (not (equal? (length args) (length (upnp-service-action-args a))))
                (error 'service-wrapper "Invalid arguments for operation ~a; expected ~v"
                       op
                       (upnp-service-action-args a)))
              (define request `(s:Envelope ((xmlns:s "http://schemas.xmlsoap.org/soap/envelope/")
                                            (s:encodingStyle "http://schemas.xmlsoap.org/soap/encoding/"))
                                           (s:Body
                                            (,action-tag ((xmlns:u ,(upnp-service-type s)))
                                                         ,@(map (lambda (actual formal)
                                                                  (list (string->symbol formal)
                                                                        actual))
                                                                args
                                                                (upnp-service-action-args a))))))
              ;; (pretty-print `(REQUEST ,(upnp-service-control-url s) ,request))
              (define response
                (http-post->xml (upnp-service-control-url s)
                                (list
                                 "Content-Type: text/xml"
                                 (string-append "SOAPACTION: " (upnp-service-type s) "#" op))
                                request))
              ;; (pretty-print `(RESPONSE ,response))
              (and response
                   (let ()
                     (define body (se-path* '(s:Body) response))
                     (when (not body) (raise-upnp-error #f "Missing SOAP body in response"))
                     (cond
                       [(element-is? 's:Fault body)
                        (define code (se-path* '(errorCode) body))
                        (define desc (se-path* '(errorDescription) body))
                        (raise-upnp-error (and code (string->number code)) desc)]
                       [else
                        (for/hash ([r (se-path*/list (list response-tag) response)])
                          (match r
                            [(list name _ value) (values (symbol->string name) value)]
                            [(list name _) (values (symbol->string name) "")]))])))]))
         upnp-service-dispatcher)))

(define (in-upnp-services #:scan-time [scan-time (default-scan-time)])
  (in-generator
   (define seen (set))
   (for ([record (in-upnp-discovery #:scan-time scan-time)])
     (define location (get-header "location" record))
     (define service-ip (let ((host (upnp-discovery-response-host record)))
                          (if (ip-address? host)
                              host
                              (dns-get-address (dns-find-nameserver) host))))
     (when location
       (define base-url-string (bytes->string/latin-1 (header-value location)))
       (when (not (set-member? seen base-url-string))
	 (set! seen (set-add seen base-url-string))
	 (define base-url (string->url base-url-string))
	 (define service-description
           (with-handlers ((exn:fail? (lambda (e) #f)))
             (http-get->xml base-url)))
         ;; (pretty-print `(service-description ,service-description))
         (when service-description
           (for ([service-xml (se-path*/list '(serviceList) service-description)]
                 #:when (element-is? 'service service-xml))
             (define (extract-url path)
               (let ((relative-url (se-path* path service-xml)))
                 (and relative-url (combine-url/relative base-url relative-url))))
             (let ((wrapper (service-wrapper
                             (upnp-service (se-path* '(serviceType) service-xml)
                                           (extract-url '(controlURL))
                                           (extract-url '(eventSubURL))
                                           (extract-url '(SCPDURL))
                                           service-ip))))
               (when wrapper
                 (yield wrapper))))))))))

(define (upnp-service-type=? s type)
  (equal? (upnp-service-type (s 'descriptor)) type))

;; (require racket/pretty)
