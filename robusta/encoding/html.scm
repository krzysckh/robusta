#| doc
s-expressions → html

this library was originally written for [chai](https://github.com/krzysckh/chai)

|#
(define-library
  (robusta encoding html)

  (import
    (owl core)
    (owl syscall)
    (owl list)
    (owl list-extra)
    (owl io)
    (owl string)
    (scheme base)

    (only (robusta server) ->string))

  (export
    encode)

  (begin
    ; https://github.com/jquery/jquery-migrate/issues/444
    (define self-closing-tags
      '(area base br col embed hr img input link meta param source track wbr))

    (define (string->safe-html-string s)
      s) ; TODO: implement this

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

    (define (encode l)
      (->string
        (list
          (print-tag 'start (car l))

          (if (cdr l)
            (map
              (λ (x)
                (cond
                  ((number? x) (number->string x))
                  ((string? x) (string->safe-html-string x))
                  ((list? x) (encode x))
                  (else
                    (encode (->string x)))))
              (cdr l))
            "")

          (if (not (has? self-closing-tags (car* (car l))))
            (print-tag 'end (car l))
            "")
          )))))
