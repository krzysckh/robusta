#| doc
urlencode, urldecode
|#
(define-library
  (robusta encoding url)

  (import
   (owl toplevel)
   (owl unicode))

  (export
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
      (decode-form-f data (λ (x) x)))))
