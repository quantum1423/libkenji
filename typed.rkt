#lang typed/racket/base
(require racket/match)
(require compatibility/defmacro)
(require (for-syntax typed/racket/base))
(require/typed ffi/unsafe/atomic
               [start-breakable-atomic (-> Void)]
               [end-breakable-atomic (-> Void)])

(require/typed/provide "ports.rkt"
                       [close-port (-> Port Void)]
                       [read-bytes-avail
                        (-> Input-Port
                            (U EOF Bytes))]
                       [tcp-connect (-> String Integer (values Input-Port Output-Port))]
                       [tcp-accept (-> TCP-Listener (values Input-Port Output-Port))]
                       [tcp-serve (->* (#:port Integer #:handler
                                               (-> Input-Port Output-Port Any))
                                       (#:host String)
                                       TCP-Listener)]
                       )

(require/typed/provide "main.rkt"
                       [le->number (-> Bytes Nonnegative-Integer)])

(provide with-semaphore
         make-chan
         chan-send
         chan-recv
         chan-send-evt
         chan-recv-evt)

(define-macro (guard . rst)
  `(let ([_HANDLER : (-> Void) (λ() (void))])
     (dynamic-wind
      void
      (lambda () . ,rst)
      (lambda() (_HANDLER)))))

(define-macro (defer expr)
  `(let ([_ohd _HANDLER])
     (set! _HANDLER
           (λ() ,expr (_ohd)))))

(define-macro (yarn . rst)
  `(thread
    (lambda ()
      (guard . ,rst))))

(define-syntax-rule (with-semaphore sem exp1 ...)
  (begin
    (semaphore-wait sem)
    (dynamic-wind
     void
     (lambda ()
      exp1 ...)
     (lambda () (semaphore-post sem)))))

(require (prefix-in pf- pfds/queue/bankers))

(struct (A) fasync-channel
  ([queue : (pf-Queue A)]
   [sem-1 : Semaphore]
   [sem-2 : Semaphore]
   [lock : Semaphore])
  #:mutable)

(: make-fasync-channel (All (A) (-> Nonnegative-Integer (fasync-channel A))))
(define (make-fasync-channel n)
  (fasync-channel ((inst pf-queue A))
                  (make-semaphore 0)
                  (make-semaphore n)
                  (make-semaphore 1)))

(: fasync-channel-get-evt 
   (All (A) (-> (fasync-channel A) (Evtof A))))
(define (fasync-channel-get-evt fas)
  (match fas
    [(fasync-channel cue s1 s2 lok)
     (handle-evt s1
                 (lambda (evt)
                   (with-semaphore lok
                    (semaphore-post s2)
                    (define lol (pf-head cue))
                    (set-fasync-channel-queue! fas (pf-tail cue))
                    lol)))]))

(: fasync-channel-put-evt
   (All (A) (-> (fasync-channel A) A (Evtof Void))))
(define (fasync-channel-put-evt fas val)
  (match fas
    [(fasync-channel cue s1 s2 lok)
     (handle-evt s2
                 (lambda (evt)
                   (with-semaphore lok
                    (set-fasync-channel-queue! fas (pf-enqueue val cue))
                    (semaphore-post s1))))]))

(struct (A) Chan
  ([reader : (-> (Evtof A))]
   [sender : (-> A (Evtof Any))]))

(: make-chan (All (A) (-> Nonnegative-Integer (Chan A))))
(define (make-chan n)
  (cond
    [(zero? n) (: haha (Channelof A))
               (define haha (make-channel))
               (Chan (lambda () haha)
                     (lambda ((x : A))
                       (channel-put-evt haha x)))]
    [else (: haha (fasync-channel A))
          (define haha (make-fasync-channel n))
          (Chan (lambda () (fasync-channel-get-evt haha))
                (lambda ((x : A)) (fasync-channel-put-evt haha x)))]))

(: chan-send (All (A) (-> (Chan A) A Void)))
(define (chan-send ch val)
  (sync (chan-send-evt ch val)))

(: chan-recv (All (A) (-> (Chan A) A)))
(define (chan-recv ch)
  (sync (chan-recv-evt ch)))

(: chan-recv-evt (All (A) (-> (Chan A) (Evtof A))))
(define (chan-recv-evt ch)
  ((Chan-reader ch)))

(: chan-send-evt (All (A) (-> (Chan A) A (Evtof Void))))
(define (chan-send-evt ch val)
  (wrap-evt ((Chan-sender ch) val) void))