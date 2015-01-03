#lang racket/base
(require "control-flow.rkt")
(provide (all-from-out "control-flow.rkt"))
(require "concurrency.rkt")
(provide (all-from-out "concurrency.rkt"))
(require "misc.rkt")
(provide (all-from-out "misc.rkt"))
(require "assert.rkt")
(provide (all-from-out "assert.rkt"))
(require "logging.rkt")
(provide (all-from-out "logging.rkt"))
(require "ports.rkt")
(provide (all-from-out "ports.rkt"))

(define (memoize fun)
  (let ([cache (make-weak-hash)])
    (lambda x
      (cond
        [(hash-has-key? cache x) (hash-ref cache x)]
        [else (hash-set! cache x (apply fun x))
              (hash-ref cache x)]))))

(provide memoize)