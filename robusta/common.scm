#| doc
common functions
|#
(define-library
  (robusta common)

  (import
   (owl toplevel))

  (export
    bool->string
    ->string
    object?)

  (begin
    (define (bool->string v)
      (if v "#t" "#f"))

    (define (->string x)
      (cond
        ((list? x) (foldl string-append
                          ""
                          (map (λ (x) (string-append (->string x) " ")) x)))
        ((pair? x) (string-append (->string (car x)) " "
                                  (->string (cdr x)) " "))
        ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        ((boolean? x) (bool->string x))
        ((char? x) (string x))
        ((string? x) x)
        (else
          "")))

    (define (object? lst)
      (and
       (list? lst)
       (all pair? lst)
       (all (λ (x) (or (symbol? (car x)) (string? (car x)))) lst)))
    ))
