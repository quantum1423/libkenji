#lang racket/base
(require "misc.rkt")
(require "control-flow.rkt")
(require "concurrency.rkt")
(require racket/port)
(require data/queue)
(require racket/tcp)

#| PORT-RELATED CONVENIENCE FUNCTIONS |#

;; TCP without buffering

(define (tcp-connect/no-buffer . rst)
  (define-values (in out) (apply tcp-connect rst))
  (file-stream-buffer-mode out 'none)
  (file-stream-buffer-mode in 'none)
  (values in out))

(define (tcp-accept/no-buffer . rst)
  (define-values (in out) (apply tcp-accept rst))
  (file-stream-buffer-mode out 'none)
  (file-stream-buffer-mode in 'none)
  (values in out))

(provide (rename-out (tcp-connect/no-buffer tcp-connect)
                     (tcp-accept/no-buffer tcp-accept))
         tcp-connect/no-buffer
         tcp-accept/no-buffer)

;; Sometimes we just want to for-each close all the ports.

(define (close-port . prts)
  (for ([prt prts])
    (cond
      [(input-port? prt) (close-input-port prt)]
      [(output-port? prt) (close-output-port prt)])))

(provide close-port)

;; A "functional" read-bytes-avail is sorely lacking.

(define (read-bytes-avail prt)
  (define buffer (make-bytes 8192))
  (define toret (read-bytes-avail! buffer prt))
  (cond
    [(eof-object? toret) eof]
    [else (subbytes buffer 0 toret)]))

(provide read-bytes-avail)

;; Make an input port, the sane way!

(define (make-input-port/sane name evt read-in close)
  (define-values (cache-in cache-out) (make-pipe))
  (define buff (make-bytes 65536))
  (make-input-port
   name
   (lambda (bts)
     (with-handlers ([exn:fail? (λ(x) (close-port cache-out)
                                  (raise x))])
       (select res
         [cache-in (read-bytes-avail! bts cache-in)]
         [else
          (select res
            [evt (read-in bts)]
            [else (handle-evt evt
                              (lambda (x)
                                (define thing (read-in buff))
                                (cond
                                  [(eof-object? thing) (close-port cache-out)]
                                  [else (write-bytes buff cache-out 0 thing)])
                                cache-in))])])))
   #f
   close))

(define (make-input-port/buffered name evt read-in close)
  (define-values (cache-in cache-out) (make-pipe))
  (make-input-port
   name
   (lambda (bts)
     (with-handlers ([exn:fail? (λ(x) (close-port cache-out)
                                  (raise x))])
       (select res
         [cache-in (read-bytes-avail! bts cache-in)]
         [else
          (select res
            [evt (define hoho (read-in))
                 (cond
                   [(eof-object? hoho) (close-port cache-out)
                                       eof]
                   [(< (bytes-length hoho) (bytes-length bts))
                    (bytes-copy! bts 0 hoho)
                    (bytes-length hoho)]
                   [else (bytes-copy! bts 0 (subbytes hoho 0 (bytes-length bts)))
                         (write-bytes (subbytes hoho (bytes-length bts)) cache-out)
                         (bytes-length bts)])]
            [else (handle-evt evt
                              (lambda (x)
                                (define thing (read-in))
                                (cond
                                  [(eof-object? thing) (close-port cache-out)]
                                  [else (write-bytes thing cache-out)])
                                cache-in))])])))
   #f
   close))

(provide make-input-port/sane
         make-input-port/buffered)

;; TCP handling functions

(define (tcp-serve #:port port
                   #:host (host "127.0.0.1")
                   #:handler handler)
  (define listener (tcp-listen port 256 #t host))
  (yarn
   (let loop ()
     (define-values (in out) (with-handlers ([exn:fail? (lambda _
                                                          (yarn-suicide!))])
                               (tcp-accept/no-buffer listener)))
     (yarn
      (defer (close-port in out))
      (handler in out))
     (loop)))
  listener)

;; Safer copy-port

(define (safe-copy-port in out)
  (define hoho (make-bytes 16384))
  (let loop()
    (define lol (read-bytes-avail! hoho in))
    (cond
      [(eof-object? lol) (void)]
      [else (write-bytes hoho out 0 lol)
            (loop)])))

;(provide (rename-out (safe-copy-port copy-port)))

(provide tcp-serve)