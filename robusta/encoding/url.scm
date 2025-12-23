#| doc
urlencode, urldecode
|#
(define-library
  (robusta encoding url)

  (import
   (owl toplevel)
   (owl unicode)
   (prefix (owl parse) get-)
   (robusta common))

  (export
   encode-thing
   encode
   decode-form-nosym
   decode-form
   decode)

  (begin
    ;; (define (decode-lst l acc)
    ;;   (print (len l))
    ;;   (cond
    ;;    ((null? l) acc)
    ;;    ((eqv? (car l) #\+) (decode-lst (cdr l) (append acc '(#\space))))
    ;;    ;; ((eqv? (car l) #\%)
    ;;    ;;  (decode-lst (cdddr l) (append acc (list (read (string #\# #\x (cadr l) (caddr l)))))))
    ;;    (else
    ;;     (decode-lst (cdr l) (append acc (list (car l)))))))

    (define get-hex
      (get-parses
       ((_ (get-imm #\%))
        (a get-single-hex)
        (b get-single-hex))
       (bytes->number (list a b) 16)))

    (define get-+
      (get-parses
       ((_ (get-imm #\+)))
       #\space))

    (define get-thing
      (get-plus
       (get-one-of
        get-hex
        get-+
        (get-byte-if (B not (C = #\=))))))

    (define get-kv-pair
      (get-parses
       ((k get-thing)
        (_ (get-imm #\=))
        (v (get-either get-thing (get-epsilon #n))))
       (cons (bytes->string k) (bytes->string v))))

    (define get-kv-pairs
      (get-plus
       (get-one-of
        (get-parses
         ((p    get-kv-pair)
          (_    (get-imm #\&)))
         p)
        get-kv-pair)))

    (define parser
      get-kv-pairs)

    (define (decode s)
      (get-parse get-thing (str-iter s) #f))

    (define (decode-form-f data f)
      (if (= 0 (string-length data))
          #n
          (map
           (λ (x) (cons (f (car x)) (cdr x)))
           (get-parse parser (str-iter data) 'shit))))

    (define (decode-form data)
      (decode-form-f data string->symbol))

    ;; dont string->symbol the key
    (define (decode-form-nosym data)
      (decode-form-f data I))

    (define alphanumeric? (string->regex "m/[a-zA-Z0-9]/"))

    (define (encode-thing v)
      (fold string-append ""
            (map (λ (c) (let ((s (string c)))
                          (cond
                           ((= #\space c) "+")
                           ((alphanumeric? s) s)
                           (else
                            (string-append "%" (list->string (render-number c () 16)))))))
                 (string->bytes (str v)))))

    ;; (k . v) → k=v
    (define (encode-pair p)
      (string-append (encode-thing (car p)) "=" (encode-thing (cdr p))))

    (define (encode l)
      (let ((s (fold (λ (a b) (string-append a b "&")) "" (map encode-pair l))))
        (substring s 0 (- (string-length s) 1))))

    ))
