#lang racket/base
(require racket/match)
(require racket/async-channel)
(require racket/promise)
(require compatibility/defmacro)
(require (for-syntax racket))
(require "control-flow.rkt")
(require "assert.rkt")
(provide (all-defined-out))


;; minimize thread footprint
(current-thread-initial-stack-size 256)

;; chans: two unrelated types for async and sync channels is horrible!

(define (chan? ch)
  (or (channel? ch)
      (async-channel? ch)))

(define (chan-send ch val)
  (cond
    [(channel? ch) (channel-put ch val)]
    [(async-channel? ch) (async-channel-put ch val)]))

(define (chan-recv ch)
  (cond
    [(channel? ch) (channel-get ch)]
    [(async-channel? ch) (async-channel-get ch)]))

(define (make-chan (limit 0))
  (cond
    [(zero? limit) (make-channel)]
    [else (make-async-channel limit)]))

(define current-recv-chan (make-parameter (make-channel)))

;; (yarn ...): simple macro for libkenji-managed threads (yarns) to save typing
(define-syntax-rule (yarn exp1 ...)
  (let* (;[stevt (make-semaphore 0)]
         [x (thread
             (lambda ()
               (with-handlers ([exn:fail? (lambda (x)
                                            (printf "Unhandled exception in yarn: ~a\n" 
                                                    (exn-message x))
                                            (exit 42))])
                 (with-handlers ([exn:break? void])
                   (parameterize ([current-recv-chan (make-channel)])
                     (guard
                      ;(semaphore-post stevt)
                      exp1 ...))
                   ))
                 ))])
    ;(semaphore-wait stevt)
    x))
  
;; with-lock: 'nuff said
(define-syntax-rule (with-semaphore lck exp1 ...)
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
  (when (equal? v #f)
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

(define (yarn-recv-evt)
  (wrap-evt (thread-receive-evt)
            (lambda(x)
              (yarn-recv))))

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

;; Select statement, sugar for handle-evt

(define-macro (select rv . rst)
  `(sync
    ,@(map
       (lambda (clause)
         `(handle-evt ,(car clause)
                      (lambda (,rv)
                        ,@(cdr clause))))
       rst)))

;; Locks associated with objects

(define (get-lock obj)
  (atomic
    (cond
      [(attr-have? obj '__lock) (attr-get obj '__lock)]
      [else (attr-set! obj '__lock (make-semaphore 1))
            (get-lock obj)])))

(define-syntax-rule (with-lock-on obj exp1 ...)
  (let ([haha (get-lock obj)])
    (dynamic-wind
     (lambda()
       (semaphore-wait haha))
     (lambda ()
       exp1 ...)
     (lambda ()
       (semaphore-post haha)))))

(define (genlock)
  (box (gensym)))