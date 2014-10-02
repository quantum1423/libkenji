#lang racket

;; This file implements sockets, a much simplified version of ports.

(struct _socket
  (read write close))

(define (_ports->socket in out)
  (make-socket
   (lambda (lim)
     (define buffer (make-bytes lim))
     (define n (read-bytes-avail! buffer in))
     (cond
       [(eof-object? n) eof]
       [else (subbytes buffer 0 n)]))
   (lambda (bts)
     (write-bytes bts out))
   (lambda ()
     (close-output-port out)
     (close-input-port in))))

(define (socket-read skt (lim 8192))
  ((_socket-read skt) lim))

(define (socket-read/fixed skt lim)
  (define fst (socket-read skt lim))
  (cond
    [(= (bytes-length fst) lim) fst]
    [else (bytes-append fst
                        (socket-read/fixed skt 
                                           (- lim (bytes-length fst))))]))

(define (socket-write skt lol)
  ((_socket-write skt) lol))

(define (socket-close skt)
  ((_socket-close skt)))

(define make-socket _socket)

(define-syntax-rule (ports->socket expr)
  ((thunk
    (define-values (in out) expr)
    (_ports->socket in out))))