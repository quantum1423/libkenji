#lang racket
(require "misc.rkt")
(require "control-flow.rkt")

#| PORT-RELATED CONVENIENCE FUNCTIONS |#

;; Sometimes we just want to for-each close all the ports.

(define (close-port prt)
  (cond
    [(input-port? prt) (close-input-port prt)]
    [(output-port? prt) (close-output-port prt)]))

(provide close-port)

;; A functional read-bytes-avail is sorely lacking.

(define (read-bytes-avail prt)
  (define buffer (make-bytes 49152))
  (define toret (read-bytes-avail! buffer prt))
  (cond
    [(eof-object? toret) eof]
    [else (subbytes buffer 0 toret)]))

(provide read-bytes-avail)

;; Hard-flushes a pipe, so to speak. FIXME: Uses ugly polling currently.

(define (pipe-output-port? prt)
  (with-handlers ([exn:fail? (lambda x #f)])
    (pipe-content-length prt)
    #t))

(define (flush-output-port prt)
  (cond
    [(pipe-output-port? prt)
     (let loop ([n 0])
       (flush-output prt)
       (cond
         [(zero? (pipe-content-length prt)) (void)]
         [(< n 10) (sleep 0) (loop (add1 n))]
         [(< n 20) (sleep 0.05) (loop (add1 n))]
         [(< n 100) (sleep 0.2) (loop (add1 n))]
         [(< n 200) (sleep 1) (loop (add1 n))]
         [else (error "flush-output-port timed out. Deadlock?")]))]
    [else (flush-output prt)]))

(provide flush-output-port)