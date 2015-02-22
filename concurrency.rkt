#lang racket/base
(require racket/match)
(require racket/promise)
(require compatibility/defmacro)
(require data/heap)
(require data/queue)
(require (for-syntax racket/base))
(require "control-flow.rkt")
(require "assert.rkt")
(require "logging.rkt")
(provide (all-defined-out))


;; minimize thread footprint
(current-thread-initial-stack-size 256)

;; chans: two unrelated types for async and sync channels is horrible!

(define (chan? ch)
  (or (channel? ch)
      (fasync-channel? ch)))

(define (chan-send ch val)
  (cond
    [(channel? ch) (channel-put ch val)]
    [(fasync-channel? ch) (fasync-channel-put ch val)]))

(define (chan-recv ch)
  (cond
    [(channel? ch) (channel-get ch)]
    [(fasync-channel? ch) (fasync-channel-get ch)]))

(define (chan-send-evt ch val)
  (cond
    [(channel? ch) (channel-put-evt ch val)]
    [(fasync-channel? ch) (fasync-channel-put-evt ch val)]))

(define (make-chan (limit 0))
  (cond
    [(zero? limit) (make-channel)]
    [else (make-fasync-channel limit)]))

(define current-recv-chan (make-parameter (make-channel)))

;; (yarn ...): simple macro for libkenji-managed threads (yarns) to save typing
(define-syntax-rule (yarn exp1 ...)
  (let* ([stevt (make-semaphore 0)]
         [x (thread
             (lambda ()
               (with-handlers ([exn:break? void])
                 (parameterize ([current-recv-chan (make-channel)])
                   (guard
                    (semaphore-post stevt)
                    exp1 ...))
                 )
               ))])
    (semaphore-wait stevt)
    x))

;; suicide
(define (yarn-suicide!) (break-thread (current-thread)))

;; with-semaphore: 'nuff said
(define-syntax-rule (with-semaphore lck exp1 ...)
  (begin
    (semaphore-wait lck)
    (guard
     (defer (semaphore-post lck))
     exp1 ...)))

(define yarn-kill break-thread)

;; Yarns communicate with blocking, *bidirectional*, channels. Send returns a val.
;; Using thread-send etc is *UNSAFE*

(define (yarn-send yrn . msg)
  (when (null? msg)
    (PANIC "yarn-send cannot be called without a message!"))
  (when (= 1 (length msg))
    (set! msg (car msg)))
  (define replychan (current-recv-chan))
  (define tosend (cons msg replychan))
  (define v (thread-send yrn tosend))
  (when (equal? v #f)
    (error "yarn-send: target yarn died before receiving"))
  (match (chan-recv replychan)
    [(cons 'xaxa msg) msg]))

(define (yarn-send/async yrn . msg)
  (when (null? msg)
    (PANIC "yarn-send cannot be called without a message!"))
  (when (= 1 (length msg))
    (set! msg (car msg)))
  (define replychan (make-chan 1))
  (define tosend (cons msg replychan))
  (define v (thread-send yrn tosend))
  (when (equal? v #f)
    (error "yarn-send: target yarn died before receiving"))
  (handle-evt (choice-evt (thread-dead-evt yrn)
                          replychan)
              (lambda (res)
                (when (equal? res (thread-dead-evt yrn))
                  (error "yarn-send: target yarn died before replying"))
                (match res
                  [(cons 'xaxa msg) msg]))))

(define __yarn_last_sender (make-thread-cell #f))
(define (yarn-recv)
  (match (thread-receive)
    [(cons msg (? chan? ch)) (thread-cell-set! __yarn_last_sender ch)
                             msg]))

(define (yarn-reply msg)
  (when (not (chan? (thread-cell-ref __yarn_last_sender)))
    (error "yarn-reply: already replied"))
  (chan-send (thread-cell-ref __yarn_last_sender) (cons 'xaxa msg))
  (thread-cell-set! __yarn_last_sender #f))

(define (yarn-recv/imm)
  (let ([x (yarn-recv)])
    (yarn-reply (void))
    x))

(define (yarn-recv-evt)
  (wrap-evt (thread-receive-evt)
            (lambda(x)
              (yarn-recv))))

(define (yarn-recv/imm-evt)
  (wrap-evt (thread-receive-evt)
            (lambda(x)
              (yarn-recv/imm))))

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
  (cond
    [(equal? 'else
             (caar (reverse rst)))
     `(sync/timeout (lambda()
                      ,@(cdr (car (reverse rst))))
                    ,@(map
                       (lambda (clause)
                         `(handle-evt ,(car clause)
                                      (lambda (,rv)
                                        ,@(cdr clause))))
                       (reverse (cdr (reverse rst)))))]
    [else
     `(sync
       ,@(map
          (lambda (clause)
            `(handle-evt ,(car clause)
                         (lambda (,rv)
                           ,@(cdr clause))))
          rst))]))

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
  (box (random 1000000)))

;; A yarn whose job is to execute events on a timeline.
;; Send in lists whose first element is ms to execution, and second element is
;; task to do.
(define (make-timer-yarn (warn? #f))
  (yarn
   (define hoho (make-heap (λ(a b) (< (car a) (car b)))))
   (let loop()
     (select res
       [(if (zero? (heap-count hoho))
            never-evt
            (cdr (heap-min hoho)))
        (heap-remove-min! hoho)
        (define x (current-milliseconds))
        (res)
        (when (> (- (current-milliseconds) x) 1000)
          (when warn?
            (print-log 'warn "Timer action took more than 1 second to complete\n")))
        (loop)]
       [(yarn-recv-evt) (match res
                          [(list (? real? ms)
                                 task)
                           (define fire-time (+ (current-inexact-milliseconds)
                                                ms))
                           (heap-add! hoho
                                      (cons fire-time
                                            (handle-evt
                                             (alarm-evt
                                              fire-time)
                                             (lambda (evt)
                                               task))))
                           (yarn-reply (void))
                           (loop)]
                          ['die (yarn-reply (void))])]))))

;; An async channel that is fast

(define (fasync-channel-get-evt fas)
  (match fas
    [(fasync-channel cue s1 s2)
     (handle-evt s1
               (lambda(evt)
                 (with-lock-on fas
                   (semaphore-post s2)
                   (dequeue! cue))))]))

(struct fasync-channel (cue s1 s2)
  #:property prop:evt fasync-channel-get-evt)

(define (make-fasync-channel n)
  (fasync-channel (make-queue)
                  (make-semaphore 0)
                  (make-semaphore n)))

(define (fasync-channel-get fas)
  (sync (fasync-channel-get-evt fas)))

(define (fasync-channel-put fas val)
  (sync (fasync-channel-put-evt fas val)))

(define (fasync-channel-put-evt fas val)
  (match fas
    [(fasync-channel cue s1 s2)
     (handle-evt s2
               (lambda (evt)
                 (with-lock-on fas
                   (enqueue! cue val)
                   (semaphore-post s1))))]))

#|
(require profile)
(require profile/render-graphviz)
(profile-thunk
 (lambda () (time (yarn-bench 1000 500)))
 #:threads #t
 #:render render
 #:use-errortrace? #t)|#