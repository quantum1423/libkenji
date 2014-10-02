#lang racket

(define (make-random n)
  (with-input-from-file "/dev/urandom"
    (thunk
     (read-bytes n))))

(provide make-random)