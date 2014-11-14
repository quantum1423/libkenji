#lang racket

(define (hacky-dynamic-wind a b c)
  (a)
  (with-handlers ([exn? (λ(x) (c) (raise x))])
    (b))
  (c))

(provide (all-defined-out))