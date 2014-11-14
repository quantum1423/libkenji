#lang racket
(require "kernel.rkt")
(require racket/async-channel)

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
    (fchannel-get last-ch))
  )

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
        (sync
         (async-channel-put-evt
          nxt (sync ch)))))))
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

(llthread
(for* ([n (list 10 100 1000)]
       [m (list 10 100 1000)])
  (collect-garbage)
  (printf "~a threads, ~a messages\n" n m)
  (printf "STOCK:\t\t")
  (time (lolo n m))
  (collect-garbage)
  (printf "LIBKENJI:\t")
  (time (haha n m))
  (newline)
  (sleep 1)
  (collect-garbage)
  ))

(start-yarn-kernel)