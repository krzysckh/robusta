#| doc
common functions
|#
(define-library
  (robusta common)

  (import
   (owl toplevel)
   (prefix (owl parse) get-))

  (export
   content-length
   lowercase
   flatten
   hex?
   get-single-hex
   bytes->number
   bool->string
   ->string
   object?)

  (begin
    (define (flatten l)
      (cond
       ((null? l) #n)
       ((pair? (car* l)) (append (flatten (car l)) (flatten (cdr l))))
       ((pair? l) (cons (flatten (car l)) (flatten (cdr l))))
       (else l)))

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
       (all (λ (x) (or (symbol? (car x)) (string? (car x)))) lst)
       ))
       ;; (all (B not pair?) (map cdr lst))))


    (define hex? (string->regex "m/[0-9a-fA-F]/"))

    (define get-single-hex
      (get-byte-if (λ (c) (hex? (string c)))))

    ;; from (owl sexp)
    (define (bytes->number digits base)
      (fold
       (λ (n digit)
         (+ (* n base)
            (fxand (if (lesser? #\9 digit) (lets ((d _ (fx- digit 7))) d) digit) 15)))
       0 digits))

    (define (lowercase s)
      (string-map
       (λ (c)
         (if (and (>= c #\A) (<= c #\Z))
             (+ c (- #\a #\A))
             c))
       s))

    (define (content-length c)
      (cond
       ((string? c)     (string-length c))
       ((list? c)       (len c))
       ((bytevector? c) (bytevector-length c))
       (else #f)))

    ))
