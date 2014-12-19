#lang racket/base
(require data/bit-vector)
(require racket/match)
(provide make-bloom
         bloom-add!
         bloom-has?)

(struct bloom (vec m k) #:transparent)

(define (make-bloom m k)
  (bloom (make-bit-vector (expt 2 m))
         (expt 2 m)
         (for/list ([i (in-range k)])
           (lambda (x)
             (equal-secondary-hash-code
              (* i (equal-hash-code x)))))))

(define (bloom-add! blm el)
  (match blm
    [(bloom vec m k)
     (for ([h (in-list k)])
       (bit-vector-set! vec
                        (bitwise-and (sub1 m)
                                     (h el))
                        #t))]))

(define (bloom-has? blm el)
  (match blm
    [(bloom vec m k)
     (for/and ([h (in-list k)])
       (bit-vector-ref vec
                       (bitwise-and (sub1 m)
                                    (h el))))]))