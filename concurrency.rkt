#lang racket/base
(require racket/match)
(require racket/async-channel)
(require "control-flow.rkt")
(require "assert.rkt")
(provide (all-defined-out))

(current-thread-initial-stack-size 128)

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

(define current-recv-chan (make-parameter (make-channel)))

;; (yarn ...): simple macro for libkenji-managed threads (yarns) to save typing
(define-syntax-rule (yarn exp1 ...)
  (let ([x (thread
            (lambda ()
              (with-handlers ([exn:break? void])
                (parameterize ([current-recv-chan (make-channel)])
                  (guard
                   exp1 ...)))))])
    x))
  
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
  (define replychan (current-recv-chan))
  (define tosend (cons msg replychan))
  (define v (thread-send yrn tosend))
  (when (equal? v #\f)
    (error "yarn-send: target yarn died before receiving"))
  (define res (sync replychan
                    (thread-dead-evt yrn)
                    ))
  (when (equal? res (thread-dead-evt yrn))
    (error "yarn-send: target yarn died before replying"))
  (match res
    [(cons 'xaxa msg) msg]))

(define __yarn_last_sender (make-thread-cell #f))
(define (yarn-recv)
  (match (thread-receive)
    [(cons msg (? channel? ch)) (thread-cell-set! __yarn_last_sender ch)
                                msg]))

(define (yarn-reply msg)
  (when (not (channel? (thread-cell-ref __yarn_last_sender)))
    (error "yarn-reply: already replied"))
  (channel-put (thread-cell-ref __yarn_last_sender) (cons 'xaxa msg))
  (thread-cell-set! __yarn_last_sender #f))

(define (yarn-recv/imm)
  (let ([x (yarn-recv)])
    (yarn-reply (void))
    x))

(define (yarn-bench n m)
  (imprison
   (define str
     (let loop ([nxt (yarn
                      (let loop()
                        (yarn-recv)
                        (yarn-reply 0)
                        (loop)))]
                [i 0])
       (cond
         [(= i (sub1 n)) nxt]
         [else (loop (yarn
                      (let loop()
                        (yarn-reply (yarn-send nxt (yarn-recv)))
                        (loop)))
                     (add1 i))])))
   (for ([i m])
     (yarn-send str i))))