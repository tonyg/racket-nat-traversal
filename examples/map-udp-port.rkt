#lang racket

(require "../high-level.rkt")

(define port (match (current-command-line-arguments)
               [(vector s) (string->number s)]
               [_ 5999]))

(define mapping-manager
  (mapping-change-listener 'udp
                           "0.0.0.0"
                           port
                           (lambda (assignments)
                             (pretty-print `(assignments . ,(set->list assignments))))))

(let loop ()
  (printf "Mapping manager for port ~a running...\n" port)
  (flush-output)
  (sleep 5)
  (loop))
