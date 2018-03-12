#lang setup/infotab
(define name "nat-traversal")
(define blurb
  (list
   `(p "Implementation of NAT traversal utilities for opening ports on home routers.")))
(define categories '(net))
(define can-be-loaded-with 'all)
(define homepage "https://github.com/tonyg/racket-nat-traversal")
(define primary-file "main.rkt")
(define repositories '("4.x"))
(define scribblings '(("scribblings/nat-traversal.scrbl" ())))

(define deps '("base" "bitsyntax" "web-server-lib"))
(define build-deps '("racket-doc" "scribble-lib"))
