#lang racket/base
(require "logging.rkt")
(require racket/set)
(require racket/match)

;; Big will executor

(define grand-executor (make-will-executor))
(define last-dur #f)

(void
 (thread
  (lambda ()
   (let loop ([n 1])
     (will-execute grand-executor)
     (print-log 'debg "executed will #~a in ~a µs" n (inexact->exact (floor (* last-dur 1000))))
     (loop (add1 n))))))

(define (set-finalizer! obj proc)
  (will-register grand-executor obj (lambda(x)
                                      (define tm (current-inexact-milliseconds))
                                      (proc x)
                                      (set! last-dur (- (current-inexact-milliseconds) tm)))))

(provide set-finalizer!)

;; Little endian bignums

(define (number->le num len)
  (define lst
    (let loop ([n num])
      (cond
        [(zero? n) '()]
        [else (cons (modulo n 256)
                    (loop (quotient n 256)))])))
  (bytes-append
   (list->bytes lst)
   (make-bytes (- len (length lst)))))

(define (le->number bts)
  (for/sum ([dig bts]
            [pwr (bytes-length bts)])
    (* dig (expt 256 pwr))))

(provide number->le
         le->number)

;; Little endian shortcuts for machine

(define (number->le/fast num len)
  (integer->integer-bytes num len #f #f))

(define (le->number/fast bts)
  (integer-bytes->integer bts #f #f))

(provide number->le/fast
         le->number/fast)

;; xor bytes

(define (bytes-xor bt1 bt2)
  (list->bytes
   (for/list ([x bt1]
              [y bt2])
     (bitwise-xor x y))))

(provide bytes-xor)

;; Prints a tree. AUGHHH!!! MUST WASH MY HANDS!!!

(define (print-tree tr (depth 0) (last #f) (skip (set)))
  (for ([i (in-range 0 (max 0 (sub1 depth)))])
    (if (set-member? skip i)
        (display "    ")
    (display "│   ")))
  
  (when (not (zero? depth))
    (if last
        (begin
          (display "└── ")
          (set! skip (set-add skip (- depth 1))))
        (display "├── ")))
  
  (match tr
    ['() (newline)]
    [`(,ele . ,rst) (displayln ele)
                    (for ([el rst]
                          [i (length rst)])
                      (cond
                        [(= i (sub1 (length rst))) (print-tree el (add1 depth)
                                                               #t skip)]
                        [else (print-tree el (add1 depth)
                                          #f skip)]))]))

(provide print-tree)

;; Functional deque.

(struct deque (a b) #:prefab)

(define empty '())

(define (make-deque)
  (deque empty empty))

(define (deque-get-front deq)
  (match deq
    [(deque '() '()) (error "Cannot get-front an empty deque")]
    [(deque '() rst) (deque-get-front (deque (reverse rst) empty))]
    [(deque a b) (car a)]))

(define (deque-not-front deq)
  (match deq
    [(deque '() '()) (error "lel")]
    [(deque '() rst) (deque-not-front (deque (reverse rst) empty))]
    [(deque a b) (deque (cdr a) b)]))

(define (deque-put-front deq el)
  (match deq
    [(deque a b) (deque (cons el a) b)]))

(define (deque-get-back deq)
  (match deq
    [(deque '() '()) (error "Cannot get-back an empty deque")]
    [(deque lel '()) (deque-get-back (deque '() (reverse lel)))]
    [(deque a b) (car b)]))

(define (deque-not-back deq)
  (match deq
    [(deque '() '()) (error "lel")]
    [(deque rst '()) (deque-not-back (deque empty (reverse rst)))]
    [(deque a b) (deque a (cdr b))]))

(define (deque-put-back deq el)
  (match deq
    [(deque a b) (deque a (cons el b))]))

(define (deque->list deq)
  (match deq
    [(deque a b) (append a (reverse b))]))

(provide make-deque
         deque-get-front
         deque-not-front
         deque-put-front
         deque-get-back
         deque-not-back
         deque-put-back
         deque->list)