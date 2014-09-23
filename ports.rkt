#lang racket

#| PORT-RELATED CONVENIENCE FUNCTIONS |#

;; Sometimes we just want to for-each close all the ports.

(define (close-port prt)
  (cond
    [(input-port? prt) (close-input-port prt)]
    [(output-port? prt) (close-output-port prt)]))

(provide close-port)

;; A functional read-bytes-avail is sorely lacking.

(define (read-bytes-avail prt)
  (define buffer (make-bytes 8192))
  (define toret (read-bytes-avail! buffer))
  (cond
    [(eof-object? toret) eof]
    [else (subbytes buffer 0 toret)]))

(provide read-bytes-avail)