#| doc
urlencode, urldecode
|#
(define-library
  (robusta encoding url)

  (import
   (owl toplevel)
   (owl unicode)
   (only (robusta common) object?))

  (export
   encode
   decode-form-nosym
   decode-form
   decode)

  (begin
    (define (decode-lst l acc)
      (cond
       ((null? l) acc)
       ((eqv? (car l) #\+) (decode-lst (cdr l) (append acc '(#\space))))
       ((eqv? (car l) #\%)
        (decode-lst (cdddr l) (append acc (list (read (string #\# #\x (cadr l) (caddr l)))))))
       (else
        (decode-lst (cdr l) (append acc (list (car l)))))))

    (define (decode s)
      (list->string (utf8-decode (decode-lst (string->list s) '()))))

    (define (decode-form-f data f)
      (let ((things ((string->regex "c/&/") data)))
        (map (λ (s)
               (let* ((vs ((string->regex "c/=/") s))
                      (k (f (car vs)))
                      (v (cadr vs)))
                 (cons k (decode v))))
             things)))

    (define (decode-form data)
      (decode-form-f data string->symbol))

    ;; dont string->symbol the key
    (define (decode-form-nosym data)
      (decode-form-f data (λ (x) x)))

    (define alphanumeric? (string->regex "m/[a-zA-Z0-9]/"))

    (define (encode-thing v)
      (fold string-append ""
            (map (λ (c) (let ((s (string c)))
                          (cond
                           ((= #\space c) "+")
                           ((alphanumeric? s) s)
                           (else
                            (string-append "%" (list->string (render-number c () 16)))))))
                 (string->list (str v)))))

    ;; (k . v) → k=v
    (define (encode-pair p)
      (string-append (encode-thing (car p)) "=" (encode-thing (cdr p))))

    (define (encode l)
      (let ((s (fold (λ (a b) (string-append a b "&")) "" (map encode-pair l))))
        (substring s 0 (- (string-length s) 1))))

    ))
