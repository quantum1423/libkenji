#lang racket/base
(require racket/control)

(define __scheduler #f)

(define (run-scheduler)
  (define (susp)
    (let/cc k
      (set! __scheduler k)
      (abort (void))))
  (for ([i 1000])
    (susp)
    (displayln i)))