#lang racket
(require "../assert.rkt")

(define (bytes-equal? bt1 bt2)
  (assert (= (bytes-length bt1)
             (bytes-length bt2))
          "Cannot do constant-time compare on byte strings of unequal length")
  (zero?
   (apply bitwise-ior
          (for/list ([i bt1]
                     [j bt2])
            (bitwise-xor i j)))))

(provide bytes-equal?)