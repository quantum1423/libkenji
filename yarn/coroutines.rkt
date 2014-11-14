#lang racket

(struct coroutine (cont last-resumer) #:mutable #:transparent)
(provide (struct-out coroutine))
(define current-coroutine (make-parameter #f))
(provide current-coroutine)

(define (make-coroutine thnk)
  (define toret (coroutine #f (current-coroutine)))
  (set-coroutine-cont! toret
                       (lambda ()
                         (parameterize ([current-coroutine toret])
                           (thnk))))
  toret)

(provide make-coroutine)

(define (cor-resume other-cor)
  (let/cc k
    (set-coroutine-last-resumer! other-cor (current-coroutine))
    (when (current-coroutine)
      (set-coroutine-cont! (current-coroutine) k))
    ((coroutine-cont other-cor))))

(provide cor-resume)

(define (capture-current-coroutine)
  (let/cc k
    (when (current-coroutine)
      (set-coroutine-cont! (current-coroutine) k))
    (k)))

(provide capture-current-coroutine)