#lang racket

;; pvecs, persistent vectors based on braun trees. 
;; support prepend, O(n) traversal

(define (pvec-ref btr idx)
  (cond
    [(zero? idx) (first btr)]
    [(odd? idx) (pvec-ref (second btr)
                          (quotient (sub1 idx) 2))]
    [(even? idx) (pvec-ref (third btr)
                           (quotient (sub1 idx) 2))]))

(define (pvec-set btr idx b)
  (match btr
    [(list a s t)
     (cond
       [(zero? idx) (list b s t)]
       [(odd? idx) (pvec-set (second btr)
                             (quotient (sub1 idx) 2) b)]
       [(even? idx) (pvec-set (third btr)
                              (quotient (sub1 idx) 2) b)])]))

(define (pvec-cons b btr)
  (match btr
    ['() (list b empty empty)]
    [(list a s t) (list b (pvec-cons a t) s)]))

(define (pvec-traverse btr)
  (match btr
    ['() void]
    [(list i l r) (displayln i)
                  (pvec-traverse l)
                  (pvec-traverse r)]))