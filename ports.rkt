#lang racket/base
(require "misc.rkt")
(require "control-flow.rkt")
(require "concurrency.rkt")
(require racket/port)

#| PORT-RELATED CONVENIENCE FUNCTIONS |#

;; Sometimes we just want to for-each close all the ports.

(define (close-port prt)
  (cond
    [(input-port? prt) (close-input-port prt)]
    [(output-port? prt) (close-output-port prt)]))

(provide close-port)

;; A "functional" read-bytes-avail is sorely lacking.

(define (read-bytes-avail prt (timeout 10))
  (define buffer (make-bytes 49152))
  (define toret (sync/timeout/enable-break
                 timeout
                 (read-bytes-avail!-evt buffer prt)))
  (cond
    [(not toret) eof]
    [(eof-object? toret) eof]
    [else (subbytes buffer 0 toret)]))

(provide read-bytes-avail)

;; A saner pipe. Writes buffer, and flushes, well, flush the buffer.
;; The public interface is named "tube", lol.

(define (make-tube)
  (define sp (make-sane-pipe))
  (define in
    (make-input-port/read-to-peek
     'tube-in
     (lambda (bts)
       (sane-pipe-read! sp bts))
     #f
     (lambda ()
       (sane-pipe-close sp))))
  (define out
    (make-output-port
     'tube-out
     (sane-pipe-out sp)
     (lambda (to-write
              start-pos
              end-pos
              flush?
              lel)
       (define x (sane-pipe-write sp to-write start-pos end-pos))
       x)
     (lambda()
       (sync/enable-break (sane-pipe-flush sp))
       (sane-pipe-close sp))))
  (values in out))

(provide make-tube)

(struct sane-pipe
  (flushed-lk ; Semaphore for "flushed" status. 1 means "flushed", 0 means "still has dirty data"
   in ; actual pipe
   out
   ))

(require racket/generator)

(define (make-sane-pipe)
  (define-values (in out) (make-pipe 65536))
  (sane-pipe (make-semaphore 1)
             in out))

(define (sane-pipe-write sp bts a b)
  (cond
    [(not (zero? (- b a)))
     ; Set status to "dirty" when "flushed" semaphore can be getted.
     ; If already dirty, stay dirty!
     (semaphore-try-wait? (sane-pipe-flushed-lk sp))
     (define lel (write-bytes bts (sane-pipe-out sp) a b))
     lel]
    [else 0]))

(define (sane-pipe-read! sp bts)
  (define toret (read-bytes-avail!/enable-break bts (sane-pipe-in sp)))
  (atomic
    ; Set status to flushed if nothing left!
    (when (zero? (pipe-content-length (sane-pipe-in sp)))
      ; But we shouldn't flush something already flushed
      (when (not (sync/timeout 0 (semaphore-peek-evt (sane-pipe-flushed-lk sp))))
        (semaphore-post (sane-pipe-flushed-lk sp)))))
  toret)

(define (sane-pipe-flush sp)
  (if (zero? (atomic (pipe-content-length (sane-pipe-in sp))))
      always-evt
      (begin
        (semaphore-peek-evt (sane-pipe-flushed-lk sp)))))

(define (sane-pipe-close sp)
  ;; just unlock everything, *probably* will work
  (for ([i 1000])
    (semaphore-post (sane-pipe-flushed-lk sp)))
  (close-output-port (sane-pipe-out sp)))

;; With timeout

(define (read-bytes/timeout lel in (timeout 30))
  (cond
    [(port-provides-progress-evts? in)
     (define lol (sync/timeout/enable-break timeout (read-bytes-evt lel in)))
     (when (not lol)
       (error "Timeout"))
     lol]
    [else (read-bytes lel in)]))

(provide read-bytes/timeout)