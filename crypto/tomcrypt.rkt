#lang racket
(require ffi/unsafe
         ffi/unsafe/define)
(require racket/runtime-path)
(require "../control-flow.rkt")
(require "../ports.rkt")
(require "../misc.rkt")

(define-runtime-path libtomcrypt-location 
  (case (system-type)
    [(unix) (case (system-type 'word)
              [(64) "libtomcrypt-amd64.so"]
              [(32) "libtomcrypt-i386.so"])]
    [(windows) "libtomcrypt.dll"]))

(define-ffi-definer define-tc
  (ffi-lib libtomcrypt-location))

;; Cipher descriptor structure

(define-cstruct _cipher_desc
  ((name _string)
   (ID _byte)
   (min_key_length _int)
   (max_key_length _int)
   (block_length _int)
   (default_rounds _int)
   (setup _pointer)
   (ecb_encrypt _pointer)
   (ecb_decrypt _pointer)
   (test _pointer)
   (done _pointer)
   (keysize _pointer)
   
   (accel_ecb_encrypt _pointer)
   (accel_ecb_decrypt _pointer)
   (accel_cbc_encrypt _pointer)
   (accel_cbc_decrypt _pointer)
   (accel_ctr_encrypt _pointer)
   (accel_ctr_decrypt _pointer)
   (accel_lrw_encrypt _pointer)
   (accel_lrw_decrypt _pointer)
   (accel_ccm_memory _pointer)
   (accel_gcm_memory _pointer)
   (omac_memory _pointer)
   (xcbc_memory _pointer)
   (f9_memory _pointer))
  #:malloc-mode 'atomic-interior)

(define-tc find_cipher (_fun _string -> _int))
(define-tc error_to_string (_fun _int -> _string))

;; Cipher descriptors

(define-tc blowfish_desc _cipher_desc)
(define-tc twofish_desc _cipher_desc)
(define-tc xtea_desc _cipher_desc)
(define-tc aes_desc _cipher_desc)
(define-tc register_cipher (_fun _pointer -> _int))
(for-each register_cipher
          (list blowfish_desc
                twofish_desc
                xtea_desc
                aes_desc))

;; CTR mode

(define-tc ctr_start
  (_fun _int
        _pointer ; IV
        _pointer ; key
        _int     ; keylength
        _int     ; rounds
        _int     ; ctr mode
        _pointer ; ctx
        -> _int))

(define-tc ctr_done
  (_fun _pointer -> _int))

(define-tc ctr_encrypt
  (_fun _pointer ; in
        _pointer ; out
        _long    ; length
        _pointer ; ctx
        -> _int))

(define (make-ctr-chugger #:key key #:iv (iv (make-bytes 100)) 
                          #:cipher (name "twofish"))
  (define ctx (malloc 'atomic 16384)) ; 16K should be enough for anything
  (define rc (ctr_start (find_cipher name)
                        iv
                        key
                        (bytes-length key)
                        0
                        0
                        ctx))
  (when (not (zero? rc))
    (error (error_to_string rc)))
  (set-finalizer! ctx (lambda (ctx)
                        (ctr_done ctx)))
  
  (lambda (lol)
    (define tgt (make-bytes (bytes-length lol)))
    (define ec
      (ctr_encrypt lol tgt (bytes-length lol) ctx))
    (when (not (zero? rc))
      (error (error_to_string rc)))
    tgt))

(provide make-ctr-chugger)

;; SOBER-128

(define-tc sober128_start
  (_fun _pointer ; ctx
        -> _int))

(define-tc sober128_add_entropy
  (_fun _pointer ; key
        _int     ; keylen
        _pointer ; ctx
        -> _int))

(define-tc sober128_read
  (_fun _pointer ; buffer
        _int     ; buflen
        _pointer ; ctx
        -> _int))

(define (make-sober128-chugger #:key key)
  (define ctx (malloc 'atomic 16384))
  (define rc (sober128_start ctx))
  (when (not (zero? rc))
    (error (error_to_string rc)))
  
  (define lol (sober128_add_entropy key (bytes-length key)
                                    ctx))
  (when (not (zero? lol))
    (error (error_to_string lol)))
  
  (lambda(x)
    (define other (bytes-copy x))
    (define len (sober128_read other (bytes-length other)
                               ctx))
    (when (not (= len (bytes-length other)))
      (error "SOBER128 read error"))
    other))

(provide make-sober128-chugger)