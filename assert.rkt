#lang racket/base

(define-syntax assert
  (syntax-rules ()
    [(_ cond) (when (not cond)
                (error "Assertion failed" (quote cond)))]
    [(_ cond msg) (when (not cond)
                    (error msg))]))

(provide (all-defined-out))