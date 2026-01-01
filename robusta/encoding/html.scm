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

    (define (maybe-close tag self-closing-tags)
      (if (has? self-closing-tags tag)
          ""
          (str "</" tag ">")))

    (define (string->quote-quotes-string s)
      (str-replace s "\"" "\\\""))

    (define (encode* lst self-closing-tags)
      (let walk ((it lst))
        (cond
         ((string? it) it)
         ((number? it) it)
         ((symbol? it) it)
         ((list? (car it))
          (str
           "<" (caar it) " "
           (fold (λ (a b) (str a (car b) "=\"" (string->quote-quotes-string (cdr b)) "\" ")) "" (cdar it))
           ">"
           (fold str "" (map walk (cdr it)))
           (maybe-close (caar it) self-closing-tags)
           ))
         (else
          (str
           "<" (car it) ">"
           (fold string-append "" (map walk (cdr it)))
           (maybe-close (car it) self-closing-tags))
          ))))

    (define encode (C encode* self-closing-tags))

    ))
