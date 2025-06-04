#| doc
s-expressions → html

this library was originally written for [chai](https://github.com/krzysckh/chai) but development will continue here.
|#
(define-library
    (robusta encoding html)

  (import
   (owl toplevel)
   (only (robusta common) ->string))

  (export
   encode*
   encode)

  (begin
    ;; https://github.com/jquery/jquery-migrate/issues/444
    (define self-closing-tags
      '(area base br col embed hr img input link meta param source track wbr))

    ;; TODO: implement this
    (define (string->safe-html-string s)
      s)

    (define (string->quote-quotes-string s)
      (str-replace s "\"" "\\\""))

    (define (print-opts l)
      (->string
       (map (λ (x) (string-append
                    (->string (car x))
                    "=\""
                    (string->quote-quotes-string (cdr x))
                    "\"")) l)))

    (define (print-tag T v)
      (if (and (list? v) (eq? 'start T))
          (string-append
           (if (eq? 'end T) "</" "<")
           (->string (car v))
           " "
           (print-opts (cdr v))
           ">\n")
          (string-append
           (if (eq? 'end T) "</" "<")
           (->string (car* v))
           ">\n")))

    (define (encode* l self-closing-tags)
      (->string
       (list
        (print-tag 'start (car l))

        (if (cdr l)
            (map
             (λ (x)
               (cond
                ((number? x) (number->string x))
                ((string? x) (string->safe-html-string x))
                ((list? x) (encode* x self-closing-tags))
                (else
                 (encode* (->string x) self-closing-tags))))
             (cdr l))
            "")

        (if (not (has? self-closing-tags (car* (car l))))
            (print-tag 'end (car l))
            ""))))

    (define (encode l)
      (encode* l self-closing-tags))
    ))
