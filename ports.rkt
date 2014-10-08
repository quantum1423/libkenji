#lang racket
(require "misc.rkt")
(require "control-flow.rkt")
(require "concurrency.rkt")

#| PORT-RELATED CONVENIENCE FUNCTIONS |#

;; Sometimes we just want to for-each close all the ports.

(define (close-port prt)
  (cond
    [(input-port? prt) (close-input-port prt)]
    [(output-port? prt) (close-output-port prt)]))

(provide close-port)

;; A "functional" read-bytes-avail is sorely lacking.

(define (read-bytes-avail prt)
  (define buffer (make-bytes 49152))
  (define toret (read-bytes-avail! buffer prt))
  (cond
    [(eof-object? toret) eof]
    [else (subbytes buffer 0 toret)]))

(provide read-bytes-avail)

;; A saner pipe. Writes buffer, and flushes, well, flush the buffer.
;; The public interface is named "tube", lol.

(define (make-tube)
  (define sp (make-sane-pipe))
  (define in
    (make-input-port
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
       (define x (sane-pipe-write sp (subbytes to-write start-pos end-pos)))
       (when flush? (sane-pipe-flush sp))
       x)
     (lambda()
       (sane-pipe-flush sp)
       (sane-pipe-close sp))))
  (values in out))


(struct sane-pipe
  (flushed-lk ; Semaphore for "flushed" status. 1 means "flushed", 0 means "still has dirty data"
   mutex-lk ; Semaphore for mutual exclusion of critical sections
   in ; actual pipe
   out
   ))

(define (make-sane-pipe)
  (define-values (in out) (make-pipe))
  (sane-pipe (make-semaphore 1)
             (make-semaphore 1)
             in out))

(define (sane-pipe-write sp bts)
  (with-lock (sane-pipe-mutex-lk sp)
    ; Set status to "dirty" when "flushed" semaphore can be getted.
    ; If already dirty, stay dirty!
    (semaphore-try-wait? (sane-pipe-flushed-lk sp)))
  (write-bytes bts (sane-pipe-out sp)))

(define (sane-pipe-read! sp bts)
  (define toret (read-bytes-avail! bts (sane-pipe-in sp)))
  (with-lock (sane-pipe-mutex-lk sp)
    ; Set status to flushed if nothing left!
    (when (zero? (pipe-content-length (sane-pipe-in sp)))
      ; But we shouldn't flush something already flushed
      (when (not (sync/timeout 0 (semaphore-peek-evt (sane-pipe-flushed-lk sp))))
        (semaphore-post (sane-pipe-flushed-lk sp)))))
  toret)

(define (sane-pipe-flush sp)
  (sync (semaphore-peek-evt (sane-pipe-flushed-lk sp))))

(define (sane-pipe-close sp)
  ;; just unlock everything, *probably* will work
  (for ([i 1000])
    (semaphore-post (sane-pipe-flushed-lk sp)))
  (close-output-port (sane-pipe-out sp)))