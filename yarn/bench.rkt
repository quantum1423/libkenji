#lang racket
(require "kernel.rkt")
(require racket/async-channel)
(require profile)

(current-thread-initial-stack-size 100)

(define (ring-bench n m)
  (define last-ch (make-fchannel))
  (define strt
    (let loop ([nxt last-ch]
               [i 0])
      (define ch (make-fchannel))
      (define thr
        (llthread
         (for ([j m])
           (define q (fchannel-get ch))
           (fchannel-put nxt
                         q))))
      (cond
        [(= i (sub1 n)) ch]
        [else (loop ch (add1 i))])))
  (for ([i m])
    (fchannel-put strt i)
    (fchannel-get last-ch)))

(define (ring-bench/stock n m)
  (define last-ch (make-async-channel))
  (define strt
    (let loop ([nxt last-ch]
             [i 0])
    (define ch (make-async-channel))
    (define thr
      (thread
       (thunk
        (for ([i m])
          (async-channel-put nxt
                       (async-channel-get ch))))))
    (cond
      [(= i n) ch]
      [else (loop ch (add1 i))])))
  (for ([i m])
    (async-channel-put strt i)
    (async-channel-get last-ch)))


(define (haha n m)
  (ring-bench n m))

(define (lolo n m)
  (ring-bench/stock n m))