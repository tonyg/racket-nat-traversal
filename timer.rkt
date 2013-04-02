#lang racket/base
;; Timer utilities.

(provide timer-evt)

(define (timer-evt delay-seconds)
  (alarm-evt (+ (current-inexact-milliseconds) (* delay-seconds 1000.0))))
