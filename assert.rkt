#lang racket/base
(require syntax/location)
(require syntax/srcloc)
(require "logging.rkt")

(define-syntax assert
  (syntax-rules ()
    [(_ cond) (when (not cond)
                (PANIC "Assertion failed: ~a" (quote cond)))]))

(provide (all-defined-out))