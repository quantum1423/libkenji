#lang racket/base
(require "misc.rkt")
(require "control-flow.rkt")
(require "concurrency.rkt")
(require racket/port)
(require data/queue)

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
  (define datbuff #f)
  (define tname (gensym 'tube))
  ;; This semaphore fires when data is available
  (define data-avail-sem (make-semaphore 0))
  ;; This semaphore fires when data is gone
  (define data-gone-sem (make-semaphore 1))
  ;; This tells readers to give up
  (define no-more-reads #f)
  
  (define inport
    (make-input-port
     ;; name
     tname
     ;; read-in
     (let()
       (define (read-to bts)
         (cond
           [no-more-reads (error "Cannot read from closed tube")]
           [(not datbuff) (wrap-evt data-avail-sem
                                    (lambda (evt)
                                      (read-to bts)))]
           [(eof-object? datbuff) eof]
           [else (define toret (min (bytes-length dest)
                                    (bytes-length datbuff)))
                 (bytes-copy! dest
                              0
                              datbuff
                              toret)
                 (set! datbuff (subbytes datbuff toret))
                 (when (zero? (bytes-length datbuff))
                   (set! datbuff #f)
                   (semaphore-post data-gone-sem))
                 toret]))
       read-to)
     ;; peek
     #f
     ;; close
     (lambda()
       (set! no-more-reads #f))))
  
  (define outport
    (make-output-port
     ;; name
     tname
     ;; evt
     data-gone-sem            