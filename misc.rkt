#lang racket

;; Big will executor

(define grand-executor (make-will-executor))

(void
 (thread
  (thunk
   (let loop()
     (will-execute grand-executor)
     (displayln "executed a will!")
     (loop)))))

(define (set-finalizer! obj proc)
  (will-register grand-executor obj proc))

(provide set-finalizer!)