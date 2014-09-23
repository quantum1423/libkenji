#lang racket
(require racket/async-channel)
(provide (all-defined-out))

#| VARIOUS CONTROL FLOW MACROS AND GOODIES |#

;; guard/defer: deferred thunks are run when the guard is exited

(define __dtor_list (make-parameter (box empty)))

(define-syntax-rule (guard exp1 ...)
  (parameterize ([__dtor_list (box empty)])
    (dynamic-wind
     void
     (thunk
      exp1 ...)
     (thunk
      (for ([el (unbox (__dtor_list))])
        (el))))))

(define-syntax-rule (defer! exp1 ...)
  (set-box! (__dtor_list) (cons (thunk exp1 ...) (unbox (__dtor_list)))))

;; (concurrent ...): simple macro for threads to save typing

(define-syntax-rule (concurrent exp1 ...)
  (thread
   (thunk
    exp1 ...)))

;; attributes: assign attributes to arbitrary objects; implemented by weak hash tables

(define __attr_table (make-weak-hash))
(define __attr_lock (make-semaphore 1))

(define (attr-set! obj key val)
  (guard
   (semaphore-wait __attr_lock)
   (defer! (semaphore-post __attr_lock))
   (when (not (hash-has-key? __attr_table obj))
     (hash-set! __attr_table obj (make-hash)))
   (hash-set! (hash-ref __attr_table obj) key val)))

(define (attr-del! obj key val)
  (guard
   (semaphore-wait __attr_lock)
   (defer! (semaphore-post __attr_lock))
   (when (not (hash-has-key? __attr_table obj))
     (hash-set! __attr_table obj (make-hash)))
   (hash-remove! (hash-ref __attr_table obj) key val)))

(define (attr-get obj key)
  (guard
   (semaphore-wait __attr_lock)
   (defer! (semaphore-post __attr_lock))
   (hash-ref (hash-ref __attr_table obj) key)))

(define (attr-have? obj key)
  (guard
   (semaphore-wait __attr_lock)
   (defer! (semaphore-post __attr_lock))
   (and (hash-has-key? __attr_table obj)
        (hash-has-key? (hash-ref __attr_table obj) key))))

;; id-evt: changes sync result to sync result + evt

(define (id-evt evt)
  (cond
    [(attr-have? evt '__id_evt) evt]
    [else (let ([x (wrap-evt evt (λ(x) (values evt x)))])
            (attr-set! x '__id_evt #t)
            x)]))

(define (sync/id evt1 . rst)
  (apply sync (map id-evt (cons evt1 rst))))

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

