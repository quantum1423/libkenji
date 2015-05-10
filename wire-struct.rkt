#lang typed/racket/base
(require (for-syntax typed/racket/base
           racket/list
           racket/pretty))
(require (for-syntax racket/match))
(require compatibility/defmacro)
(provide define-wire-struct)

(define-macro (define-wire-struct name . pairs)
  (define smname (string-downcase
                  (symbol->string (assert name symbol?))))
  (define args (cast (map car pairs) (Listof Symbol)))
  (define read-name (string->symbol (string-append "read-" smname)))
  (define write-name (string->symbol (string-append "write-" smname)))
  (define length-link
    (cast
     (for/hash ([lol pairs]
               #:when (symbol? (third lol)))
      (values (third lol) (first lol)))
     (HashTable Symbol Symbol)))
  `(begin
     (struct ,name
       ,(for/list ([i pairs])
          (match i
            [(list (? symbol? elem) ':: 1) (list elem ': 'Byte)]
            [(list (? symbol? elem) ':: 2) (list elem ': 'Integer)]
            [(list (? symbol? elem) ':: (or 4 8)) (list elem ': 'Integer)]
            [(list (? symbol? elem) ':: (? symbol? link)) (list elem ': 'Bytes)]))
       #:transparent)
     (: ,(string->symbol (string-append "build-" smname))
        (->
         ,@(append*
            (for/list ([i pairs])
              (match i
                [(list (? symbol? elem) ':: 1) (list (string->keyword
                                                      (symbol->string elem)) 'Byte)]
                [(list (? symbol? elem) ':: 2) (list (string->keyword
                                                      (symbol->string elem)) 'Integer)]
                [(list (? symbol? elem) ':: (or 4 8)) (list (string->keyword
                                                             (symbol->string elem)) 
                                                            'Integer)]
                [(list (? symbol? elem) ':: (? symbol? link)) (list (string->keyword
                                                                     (symbol->string elem))
                                                                    'Bytes)])))
         ,name))
     (define (,(string->symbol (string-append "build-" smname))
              ,@(append*
                 (for/list ([i args])
                   (list (string->keyword (symbol->string i)) i))))
       (,name . ,args))
     
     (: ,read-name (-> Input-Port ,name))
     (define (,read-name $input)
       (let* ,(for/list ([i pairs])
                (match i
                  [(list (? symbol? elem) ':: 1) (list elem
                                                       `(cast (read-byte $input) Byte))]
                  [(list (? symbol? elem) ':: 2) (list elem 
                                                       `(integer-bytes->integer
                                                         (cast (read-bytes 2 $input) Bytes)
                                                         #f
                                                         #t))]
                  [(list (? symbol? elem) ':: 4) (list elem 
                                                       `(integer-bytes->integer
                                                         (cast (read-bytes 4 $input) Bytes)
                                                         #f
                                                         #t))]
                  [(list (? symbol? elem) ':: 8) (list elem 
                                                       `(integer-bytes->integer
                                                         (cast (read-bytes 8 $input) Bytes)
                                                         #f
                                                         #t))]
                  [(list (? symbol? elem) ':: (? symbol? ref)) (list elem 
                                                                     `(cast 
                                                                       (read-bytes ,ref $input)
                                                                       Bytes))]))
         (,name . ,args)))
     
     (: ,write-name (-> ,name Output-Port Void))
     (define (,write-name $towrite $output)
       (let ,(for/list ([i args])
               (list i `(,(string->symbol (string-append (symbol->string name) "-"
                                                         (symbol->string i))) $towrite)))
        (write-bytes
         ,(cons 'bytes-append
               (for/list ([i pairs])
                 (match i
                   [(list (? symbol? elem) ':: 1)
                    (if (hash-has-key? length-link elem)
                        `(bytes (cast (bytes-length ,(hash-ref length-link elem)) Byte))
                        `(bytes ,elem))]
                   [(list (? symbol? elem) ':: (? integer? len))
                    `(integer->integer-bytes
                      ,(if (hash-has-key? length-link elem)
                           `(bytes-length ,(hash-ref length-link elem))
                           elem)
                      ,len
                      #f
                      #t)]
                   [(list (? symbol? elem) ':: _)
                    elem])))
         $output)
         (void)))))