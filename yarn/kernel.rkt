#lang racket
(require "coroutines.rkt")
(require "helpers.rkt")
(require data/queue)
(require racket/async-channel)

(define runnable-queue (make-queue))
(define blocked-tab (make-hash))

(define last-fired-evt #f)

(define scheduler
  (make-coroutine
   (lambda()
     (let loop()
     (cond
       [(and
         (queue-empty? runnable-queue)
         (zero? (hash-count blocked-tab))) (void)]
       ; Nothing blocked
       [(zero? (hash-count blocked-tab)) 
        (cor-resume (dequeue! runnable-queue)) (loop)]
       ; Have both runnable and blocked
       [(not (queue-empty? runnable-queue))
        (define evts (map car (hash->list blocked-tab)))
        (define res (apply sync/timeout (cons 0 evts)))
        (cond
          [res (define tores (hash-ref blocked-tab (car res)))
               (displayln tores)
               (hash-remove! blocked-tab res)
               (set! last-fired-evt (cdr res))
               (cor-resume tores)
               (loop)]
          [else (cor-resume (dequeue! runnable-queue))
                (loop)])]
       ; Only have blocked
       [else (define evts (map car (hash->list blocked-tab)))
             (define res (apply sync/enable-break evts))
             (define tores (hash-ref blocked-tab (car res)))
             (hash-remove! blocked-tab (car res))
             (set! last-fired-evt (cdr res))
             (cor-resume tores)
             (loop)])))))

(define (wait-on-evt evt)
  (define q (sync/timeout 0 evt))
  (cond
    [q q]
    [else 
     (hash-set! blocked-tab
                (shared
                    ((q (wrap-evt evt (λ(x) (cons q x)))))
                  q)
                (current-coroutine))
     (cor-resume scheduler)
     last-fired-evt]))

(define (yield-to-sched)
  (enqueue! runnable-queue (current-coroutine))
  (cor-resume scheduler))

(define mailbox (make-parameter #f))

(define-syntax-rule (llthread exp1 ...)
  (begin
    (let ([q (make-coroutine
              (lambda ()
                  exp1 ...))])
      (enqueue! runnable-queue
                q))))

(define (start-yarn-kernel)
  (cor-resume scheduler))

(provide mailbox
         llthread
         wait-on-evt
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
     (enqueue-front! runnable-queue (dequeue! rc))
     (yield-to-sched)]))

(provide make-fchannel
         fchannel-get
         fchannel-put)