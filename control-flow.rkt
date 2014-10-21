#lang racket/base
(define empty '())
(require racket/async-channel)
(require ffi/unsafe/atomic)
(provide (all-defined-out))

#| VARIOUS CONTROL FLOW MACROS AND GOODIES |#

;; guard/defer: deferred thunks are run when the guard is exited

(define __dtor_list (make-parameter (box empty)))

(define-syntax-rule (guard exp1 ...)
  (parameterize ([__dtor_list (box empty)])
    (dynamic-wind
     void
     (lambda ()
      exp1 ...)
     (lambda ()
      (for ([el (unbox (__dtor_list))])
        (el))))))

(define-syntax-rule (defer exp1 ...)
  (set-box! (__dtor_list) (cons (lambda () exp1 ...) (unbox (__dtor_list)))))

;; attributes: assign attributes to arbitrary objects; implemented by weak hash tables

(define __attr_table (make-weak-hash))
(define __attr_lock (make-semaphore 1))

(define (attr-set! obj key val)
  (guard
   (semaphore-wait __attr_lock)
   (defer (semaphore-post __attr_lock))
   (when (not (hash-has-key? __attr_table obj))
     (hash-set! __attr_table obj (make-hash)))
   (hash-set! (hash-ref __attr_table obj) key val)))

(define (attr-del! obj key val)
  (guard
   (semaphore-wait __attr_lock)
   (defer (semaphore-post __attr_lock))
   (when (not (hash-has-key? __attr_table obj))
     (hash-set! __attr_table obj (make-hash)))
   (hash-remove! (hash-ref __attr_table obj) key val)))

(define (attr-get obj key)
  (guard
   (semaphore-wait __attr_lock)
   (defer (semaphore-post __attr_lock))
   (hash-ref (hash-ref __attr_table obj) key)))

(define (attr-have? obj key)
  (guard
   (semaphore-wait __attr_lock)
   (defer (semaphore-post __attr_lock))
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


;; imprison: run the block in a new custodian. When control flow goes out of the block, everything is destroyed!
(define __god_custodian (current-custodian))

(define-syntax-rule (imprison exp1 ...)
  (let ([cst (make-custodian)])
    (parameterize ([current-custodian cst])
      (guard
       ;(defer (custodian-shutdown-all cst))
       (defer (for ([elem (custodian-managed-list cst __god_custodian)])
                (cond
                  [(output-port? elem) (close-output-port elem)]
                  [(input-port? elem) (close-input-port elem)]
                  [(thread? elem) (break-thread elem)])))
       exp1 ...))))


;; Atomic.

(define-syntax-rule (atomic exp1 ...)
  (begin
    (start-breakable-atomic)
    (dynamic-wind
     void
     (lambda ()
      exp1 ...)
     end-breakable-atomic)))

(provide atomic)

;; Convert fail-exception throwing to exception *returning*.

(define-syntax-rule (return-exceptions exp1 ...)
  (with-handlers ([exn:fail? (lambda (x) x)])
    exp1 ...))

(provide return-exceptions)