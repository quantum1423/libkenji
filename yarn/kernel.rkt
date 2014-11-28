#lang racket
(require "coroutines.rkt")
(require "helpers.rkt")
(require data/queue)
(require racket/async-channel)
(require racket/performance-hint)

(define runnable-queue (make-queue))

(define last-fired-evt #f)

(define scheduler
  (make-coroutine
   (lambda()
     (let loop()
       (cond
         [(queue-empty? runnable-queue) (void)]
         ; Nothing blocked
         [else
          (cor-resume (dequeue! runnable-queue)) (loop)])))))

(define-syntax-rule (llthread exp1 ...)
  (begin
    (let ([q (make-coroutine
              (lambda ()
                exp1 ...))])
      (enqueue! runnable-queue
                q))))

(begin-encourage-inline
  
  (define (start-yarn-kernel)
    (cor-resume scheduler))
  
  (define (yield-to-sched)
    (enqueue! runnable-queue (current-coroutine))
    (cor-resume scheduler))
  
  (provide llthread
           yield-to-sched
           start-yarn-kernel)
  
  
  
  ;; Fast channels
  
  (struct fchannel (vals rc))
  
  (define (make-fchannel)
    (fchannel (make-queue)
              (make-queue)))
  
  (define (fchannel-get fc)
    (match fc
      [(fchannel (? queue-empty? vals) rc)
       (enqueue! rc (current-coroutine))
       (cor-resume scheduler)
       (dequeue! vals)]
      [(fchannel val r)
       (dequeue! val)]))
  
  (define (fchannel-put fc v)
    (match fc
      [(fchannel vals (? queue-empty? rc))
       (enqueue! vals v)
       (yield-to-sched)]
      [(fchannel vals rc)
       (enqueue! vals v)
       (enqueue! runnable-queue (current-coroutine))
       (cor-resume (dequeue! rc))]))
  )

(provide make-fchannel
         fchannel-get
         fchannel-put)