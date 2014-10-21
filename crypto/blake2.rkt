#lang racket
(require ffi/unsafe
         ffi/unsafe/define)
(require racket/runtime-path)

#| SOME SIMPLE CRYPTO PRIMITIVES |#

;; blake2 provides really fast, really secure hashing.
;; blake2s for 256, blake2b for 512

(define-runtime-path libblake-location 
  (case (system-type)
    [(unix) (case (system-type 'word)
              [(64) "libblake2-amd64.so"]
              [(32) "libblake2-i386.so"])]
    [(windows) "libblake2.dll"]))

(define-ffi-definer define-blake 
  (ffi-lib libblake-location))

(define-blake blake2s
  (_fun _pointer ; out
        _pointer ; in
        _pointer ; key
        _byte    ; outlen
        _int     ; inlen
        _byte    ; keylen
        -> _int))

(define-blake blake2b
  (_fun _pointer ; out
        _pointer ; in
        _pointer ; key
        _byte    ; outlen
        _int     ; inlen
        _byte    ; keylen
        -> _int))

(define (blake2/256 msg (key #"kenji"))
  (define out (make-bytes 32))
  (define retcode
    (blake2s out msg key
             (bytes-length out)
             (bytes-length msg)
             (bytes-length key)))
  (when (not (zero? retcode))
    (error "blake2s returned error code" retcode))
  out)

(define (blake2/512 msg (key #"kenji"))
  (define out (make-bytes 64))
  (define retcode
    (blake2b out msg key
             (bytes-length out)
             (bytes-length msg)
             (bytes-length key)))
  (when (not (zero? retcode))
    (error "blake2b returned error code" retcode))
  out)

(provide blake2/256
         blake2/512)

(blake2/256 #"jsdlkfsjdf")