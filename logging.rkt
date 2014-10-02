#lang racket
(require racket/date)
(date-display-format 'iso-8601)

(define (format-log lvl str . args)
  (define dt (current-date))
  (define hours (date-hour dt))
  (define minutes (date-minute dt))
  (define seconds (date-second dt))
  (define millisec
    (quotient (date*-nanosecond dt) 1000000))
  (format "~a ~a:~a:~a.~a >>> ~a ::: ~a"
          (date->string dt)
          (~a hours #:width 2 #:align 'right #:pad-string "0")
          (~a minutes #:width 2 #:align 'right #:pad-string "0")
          (~a seconds #:width 2 #:align 'right #:pad-string "0")
          (~a millisec #:width 3 #:align 'right #:pad-string "0")
          (string-upcase (symbol->string lvl))
          (apply format (cons str args))))

(define print-log
  (lambda x
    (displayln (apply format-log x))))

(provide (all-defined-out))