#lang web-server

(struct coroutine (cont last-resumer) #:mutable)
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
   (lambda (k)
    (displayln "LEL")
    (set-coroutine-last-resumer! other-cor (current-coroutine))
    (when (current-coroutine)
      (set-coroutine-cont! (current-coroutine) k))
    (displayln "LEL")
    ((coroutine-cont other-cor))))

(provide cor-resume)

(define bounces 10000)

(define alice
  (make-coroutine
   (lambda ()
     (let loop ()
       (cor-resume bob)
       (loop)))))

(define bob
  (make-coroutine
   (lambda ()
     (call-with-web-prompt
      (lambda ()
     (displayln "BAAB")
     (for ([i bounces])
       (cor-resume alice))
     (displayln "done"))))))

(time (cor-resume bob))