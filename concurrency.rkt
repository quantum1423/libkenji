#lang racket/base
(require racket/match)
(require racket/async-channel)
(require "control-flow.rkt")
(require "assert.rkt")
(provide (all-defined-out))

;; chans: two unrelated types for async and sync channels is horrible!

(define (chan? ch)
  (or (async-channel? ch)
      (channel? ch)))

(define (chan-send ch val)
  (cond
    [(async-channel? ch) (async-channel-put ch val)]
    [(channel? ch) (channel-put ch val)]))

(define (chan-recv ch)
  (cond
    [(async-channel? ch) (async-channel-get ch)]
    [(channel? ch) (channel-get ch)]))

(define (make-chan (limit 0))
  (cond
    [(equal? 0 limit) (make-channel)]
    [else (make-async-channel limit)]))

;; (yarn ...): simple macro for libkenji-managed threads (yarns) to save typing

(define-syntax-rule (yarn exp1 ...)
  (thread
   (lambda ()
    (with-handlers ([exn:break? void])
      (guard
       exp1 ...)))))
(current-thread-initial-stack-size 100)

;; with-lock: 'nuff said
(define-syntax-rule (with-lock lck exp1 ...)
  (begin
    (semaphore-wait lck)
    (guard
     (defer (semaphore-post lck))
     exp1 ...)))

;; Yarns communicate with blocking, *bidirectional*, channels. Send returns a val.
;; Using thread-send etc is *UNSAFE*

(define (yarn-send yrn msg)
  (define replychan (make-channel))
  (define tosend (list msg replychan))
  (define v (thread-send yrn tosend))
  (when (equal? v #\f)
    (error "yarn-send: target yarn died before receiving"))
  (define res (sync replychan
                    (thread-dead-evt yrn)))
  (when (equal? res (thread-dead-evt yrn))
    (error "yarn-send: target yarn died before replying"))
  (match res
    [(list 'xaxa msg) msg]))

(define __yarn_last_sender (make-thread-cell #f))
(define (yarn-recv)
  (match (thread-receive)
    [(list msg (? channel? ch)) (thread-cell-set! __yarn_last_sender ch)
                                 msg]))

(define (yarn-reply msg)
  (when (not (channel? (thread-cell-ref __yarn_last_sender)))
    (error "yarn-reply: already replied"))
  (channel-put (thread-cell-ref __yarn_last_sender) (list 'xaxa msg))
  (thread-cell-set! __yarn_last_sender #f))

(define (yarn-recv/imm)
  (let ([x (yarn-recv)])
    (yarn-reply (void))
    x))

(define (test-yarn)
  (define adder
    (yarn
     (let loop()
       (define x (yarn-recv))
       (sleep (* 0.01 (random)))
       (yarn-reply (add1 x))
       (loop))))
  (for ([i 10])
    (yarn
     (let loop ([i 0])
       (displayln i)
       (define j (yarn-send adder i))
       (assert (= j (add1 i)))
       (loop (add1 i)))))
  (let loop ([i 0])
    (define j (yarn-send adder i))
    (assert (= j (add1 i)))
    (loop (add1 i)))
  (kill-thread adder))

;(test-yarn)