#| doc
{encoding,decoding} json

this loosely implements the ECMA-404 standard for json encoding and decoding

it can also `encode` json, where objects are defined like this:
`'((a . b) (c . d))`, so a list with key-value pairs.

https://ecma-international.org/publications-and-standards/standards/ecma-404/
|#
(define-library
  (robusta encoding json)

  (import
   (owl toplevel)
   (owl unicode)
   (owl proof)
   (prefix (owl parse) get-)
   (robusta common))

  (export
   decode
   encode)

  (begin
    (define get-whitespace
      (get-byte-if (λ (c) (has? '(#x09 #x0a #x0d #x20) c))))

    (define get-ws+
      (get-plus get-whitespace))

    (define get-ws*
      (get-star get-whitespace))

    (define get-digit
      (get-byte-if (λ (c) (and (>= c #\0) (<= c #\9)))))

    (define get-number
      (get-parses
       ((sign  (get-maybe (get-imm #\-) #\+))
        (a     (get-plus get-digit))
        (point (get-maybe
                (get-seq
                 (get-imm #\.)
                 (get-plus get-digit))
                (list #\. #\0)))
        (E     (get-maybe
                (get-seq
                 (get-byte-if (λ (c) (or (= c #\e) (= c #\E))))
                 (get-seq
                  (get-maybe (get-byte-if (λ (c) (or (= c #\-) (= c #\+)))) #\+)
                  (get-plus get-digit)))
                (list #\e #\+ 0))))
       (let* ((base  (bytes->number a 10))
              (point* (bytes->number (cdr point) 10))
              (e      (* (if (= (cadr E) #\+) 1 -1) (bytes->number (cddr E) 10))))
         (* (if (= sign #\+) 1 -1) (+ base (/ point* (expt 10 (len (cdr point))))) (expt 10 e)))))

    (define (get-imm-then imm then)
      (get-parses
       ((_ (get-imm imm)))
       then))

    (define get-escaped
      (get-parses
       ((_ (get-imm #\\))
        (v (get-one-of
            (get-imm-then #\" #\")
            (get-imm-then #\\ #\\)
            (get-imm-then #\/ #\/)
            (get-imm-then #\b #x08)
            (get-imm-then #\f #x0c)
            (get-imm-then #\n #x0a)
            (get-imm-then #\r #x0d)
            (get-imm-then #\t #x09)
            (get-parses
             ((_ (get-imm #\u))
              (a get-single-hex)
              (b get-single-hex)
              (c get-single-hex)
              (d get-single-hex))
             (print "got escaped " (list a b c d))
             (utf8-encode (list (bytes->number (list a b c d) 16)))))))
       v))

    (define get-string
      (get-parses
       ((_ get-ws*)
        (_    (get-imm #\"))
        (data (get-star
               (get-either
                get-escaped
                (get-byte-if (B not (C = #\"))))))
        (_    (get-imm #\"))
        (_    get-ws*))
       (list->string (utf8-decode (flatten data)))))

    (define (get-kv get-value)
      (get-parses
       ((k get-string)
        (_ (get-imm #\:))
        (v get-value))
       (cons k v)))

    (define (get-kvs get-value)
      (get-star
       (get-either
        (get-kv get-value)
        (get-parses
         ((p (get-kv get-value))
          (_ (get-imm #\,)))
         p))))

    (define (get-object get-value)
      (get-parses
       ((_   (get-imm #\{))
        (kvs (get-kvs get-value))
        (_   (get-imm #\})))
       kvs))

    (define (get-array get-value)
      (get-parses
       ((_  (get-imm #\[))
        (vs (get-star
             (get-either
              (get-parses
               ((v get-value)
                (_ (get-imm #\,)))
               v)
              get-value)))
        (_  (get-imm #\])))
       vs))

    (define get-value
      (let ((f (λ (self)
                 (get-parses
                  ((_ get-ws*)
                   (v (get-one-of
                       (get-object self)
                       (get-array self)
                       get-number
                       get-string
                       (get-word "true" #t)
                       (get-word "false" #f)
                       (get-word "null" #n)))
                   (_ get-ws*))
                  v))))
        (letrec ((f* (λ (a b c d) ((f f*) a b c d))))
          (f f*))))

    (define parser
      get-value)

    (define (decode s)
      (get-parse parser (str-iter s) #f))

    (define encode-string str*)

    (define (comma-ize lst)
      (cond
       ((= (len lst) 0) "")
       ((= (len lst) 1) (car lst))
       (else            (fold (λ (a b) (str a "," b)) (car lst) (cdr lst)))))

    (define (encode-kv l encode)
      (str (encode (car l)) ":" (encode (cdr l))))

    (define (encode v)
      (let ((encode-list
             (λ (lst)
               (str "[" (comma-ize (map encode lst)) "]")))
            (encode-object
             (λ (ob)
               (str "{" (comma-ize (map (C encode-kv encode) ob)) "}"))))
        (cond
         ((eqv? v 'null) "null")
         ((object? v) (encode-object v))
         ((vector? v) (encode-list (vector->list v)))
         ((list? v) (encode-list v))
         ((number? v) (number->string v))
         ((string? v) (encode-string v))
         ((symbol? v) (encode (symbol->string v)))
         ((pair? v) (encode-list v))
         ((eq? v #t) "true")
         ((eq? v #f) "false")
         (else
          (error "unexpeced type: " v) "false"))
        ))
    ))
