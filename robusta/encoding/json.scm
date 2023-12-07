#| doc
{encoding,decoding} json

this loosely implements the ECMA-404 standard for json encoding and decoding

it WILL accept broken json, and decode it correctly or incorrectly
depending on the mood, but it *should* decode *proper* json correctly.

features, not bugs, include, but are not limited to:
  - `[1,2,3 4]` → `(1 2 3 4)`
  - `[,,,,,]`   → `()`

it can also `encode` json, where objects are defined like this:
`'((a . b) (c . d))`, so a list with key-value pairs.

be warned that this implementation is **really** slow. like **proper** slow slow.
i may fix that in the future, but for now it is what it is.

https://ecma-international.org/publications-and-standards/standards/ecma-404/

me me likey accumulators
|#
(define-library
  (robusta encoding json)

  (import
    (owl eval)
    (owl core)
    (owl syscall)
    (owl io)
    (owl list)
    (owl list-extra)
    (owl regex)
    (owl port)
    (scheme base)
    (only (robusta server) ->string))

  (export
    decode
    encode)

  (begin
    ;; i don't think json-syntax-error should be fatal
    (define (json-syntax-error s . l)
      (print-to stderr (->string (list "json-syntax-error: " s)))
      (when (not (eqv? l '()))
        (print-to stderr "  additional-information:")
        (for-each
          (λ (s) (print-to stderr (string-append "    " (->string s)))) l))
      #f)

    ; this expression is used too often so i just...
    (define (+s s) (substring s 1 (string-length s)))

    ; json string → string that will fit on a string for error reporting
    (define (jsons->strerr s)
      (cond
        ((> (string-length s) 16) (substring s 0 16))
        (else s)))

    (define (digit? chr)
      (let ((c (cond
                 ((string? chr) (substring chr 0 1))
                 ((char? chr) (string chr))
                 ((number? chr) (number->string chr))
                 (else
                   (error "digit?" "unexpected type")))))
        ((string->regex "m/^[0-9]/") c)))

    ; TODO: do it as ECMA-404's fig. 4
    ;; s "" → string(n)
    (define (get-number s acc)
      (cond
        ((digit? s)
         (get-number (+s s) (string-append acc (substring s 0 1))))
        (else
          acc)))

    ; s acc → (parsed-seq next n)
    (define (parse-escape s acc)
      (let ((fc (car (string->list s))))
        (cond 
          ((and (eq? fc #\\) (null? acc)) (parse-escape (+s s) acc))
          ((eq? fc #\\) `("\\" ,(+s s) 2))
          ((eq? fc #\") `("\"" ,(+s s) 2))
          ((eq? fc #\/) `("/" ,(+s s) 2))
          ((eq? fc #\f) (json-syntax-error "not-implemented-error"
                                           "backslash f"))
          ((eq? fc #\n) `("\n" ,(+s s) 2))
          ((eq? fc #\r) `("\r" ,(+s s) 2))
          ((eq? fc #\t) `("\t" ,(+s s) 2))
          ((eq? fc #\u) (json-syntax-error "not-implemented-error"
                                           "backslash u"))
          (else
            (json-syntax-error "syntax-error"
                               "invalid escape"
                               (jsons->strerr s))))))

    ;; s "" 0 → (parsed-s skip)
    ; ECMA-404's fig. 5
    (define (get-string s acc skip-n)
      (let ((fc (car* (string->list s))))
        (cond
          ((null? fc) (json-syntax-error
                        "expected '\"'"
                        "got null?"
                        (string-append "here →" (jsons->strerr s))))
          ((eq? fc #\") `(,acc . ,(+ skip-n 1)))
          ((eq? fc #\\)
           (let* ((l (parse-escape s '()))
                  (S (list-ref l 0))
                  (next (list-ref l 1))
                  (n (list-ref l 2)))
             (get-string next (string-append acc S) (+ skip-n n))))
          (else
            (get-string (+s s) (string-append acc (string fc)) (+ skip-n 1))))))

    ;; s "" → (string(tok) | #f . skip | #f)
    (define (get-literal-tok s acc)
      (let ((fc (car (string->list s))))
        (cond
          ((or (eqv? fc #\])
               (eqv? fc #\})
               (eqv? fc #\,)
               (eqv? fc #\space))
           (if (or (string=? acc "true")
                   (string=? acc "false")
                   (string=? acc "null"))
             `(,acc . ,(string-length acc))
             `(#f . #f)))
          (else
            (get-literal-tok (+s s)
                             (string-append acc (substring s 0 1)))))))

    (define (get-array-object-skip s T)
      (letrec* ((S (if (eqv? T 'array) "[" "{"))
                (E (if (eqv? T 'array) "]" "}"))
                (f (lambda (s acc bad-acc)
                     (cond
                       ((and (string=? (substring s 0 1) E)
                             (<= bad-acc 1)) (+ acc 1))
                       ((string=? (substring s 0 1) E)
                        (f (+s s)
                           (+ 1 acc)
                           (- bad-acc 1)))
                       ((string=? (substring s 0 1) S)
                        (f (+s s)
                           (+ 1 acc)
                           (+ 1 bad-acc)))
                       (else
                         (f (+s s)
                            (+ 1 acc)
                            bad-acc))))))
        (substring s (f s 0 0) (string-length s))))

    (define (next-token s)
      (let ((fc (car (string->list s)))
            (n-re (string->regex "m/^[0-9]+/")))
        (cond
          ((n-re s)
           (substring s (string-length (get-number s "")) (string-length s)))
          ((eq? fc #\") (substring
                          s (cdr (get-string (+s s) "" 1)) (string-length s)))
          ((eq? fc #\[) (get-array-object-skip s 'array))
          ((eq? fc #\{) (get-array-object-skip s 'object))
          ((eq? fc #\space) (next-token (+s s)))
          ((car (get-literal-tok s ""))
           (substring s (string-length (car (get-literal-tok s "")))
                      (string-length s)))
          (else
            (+s s)))))

    (define (decode s)
      (letrec ((parse-array
                 (lambda (s acc)
                   (let ((fc (car (string->list s))))
                     (cond
                       ((eqv? fc #\]) acc)
                       ((or (eqv? fc #\space)
                            (eqv? fc #\tab)
                            (eqv? fc #\,))
                        (parse-array (+s s) acc))
                       (else
                         (parse-array
                           (next-token s) (append acc (list (decode s)))))))))
               (parse-object
                 ; key == expects value, adds to acc only when value given
                 ; should be called as (parse-object s '() '()) teehee :33
                 (lambda (s acc key)
                   (let ((fc (car (string->list s))))
                     (cond
                       ; expects key | } | , | #\space
                       ((eq? fc #\}) acc)
                       ((or (eq? fc #\space) (eq? fc #\,))
                        (parse-object (+s s) acc key))
                       ((null? key)
                        (let ((val (decode s)))
                          (if (string? val)
                            (parse-object (next-token s) acc val)
                            (json-syntax-error
                              "expected key as string?: "
                              (string-append "got " (->string val))
                              (string-append "here → " (jsons->strerr s))))))
                       (else
                         ; expects value
                         (cond
                           ((or (eq? fc #\space) (eq? fc #\:))
                            (parse-object (+s s)
                                          acc key))
                           (else
                             (parse-object (next-token s)
                                           (append
                                             acc
                                             (list (cons key (decode s))))
                                           '()))))))))
               (fc (car* (string->list s))))
        (cond
          ((eq? fc #\space) (decode (+s s)))
          ((digit? fc) (string->number (get-number s "")))
          ((eq? fc #\") (car (get-string (+s s) "" 0)))
          ((eq? fc #\[) (parse-array (+s s) '()))
          ((eq? fc #\{) (parse-object (+s s) '() '()))
          ((car (get-literal-tok s ""))
           (let* ((T (get-literal-tok s ""))
                  (tok (car T))
                  (skip (cdr T)))
             (cond
               ((string=? "true" tok) #t)
               ((string=? "false" tok) #f)
               ((string=? "null" tok) 'null))))
          (else
            (json-syntax-error
              "invalid char:"
              (string-append "here → " (jsons->strerr s))
              "expected: [ | { | [0-9]+ | \" | true | false | null")))))

    ; TODO: \\ \" \' \b \r \n \uxyz ECMA stuff
    (define (encode-string s)
      (string-append "\"" s "\""))

    (define (object? lst)
      (and (list? lst)
           (not (has? (map pair? lst) #f))
           (not (has? (map
                        (λ (x) (or (symbol? (car x))
                                   (string? (car x)))) lst) #f))))

    (define (encode v)
      (let ((encode-list
              (λ (lst)
                (let* ((l (apply
                            string-append
                            (map (λ (x) (string-append (encode x) "," )) lst)))
                       (L (substring l 0 (- (string-length l) 1))))
                  (string-append "[" L "]"))))
            (encode-object
              (λ (obj)
                (let* ((o (apply
                            string-append
                            (map (λ (x) (string-append
                                           (encode (->string (car x)))
                                           ":" (encode (cdr x)) ",")) obj)))
                       (O (substring o 0 (- (string-length o) 1))))
                  (string-append "{" O "}"))
                )))
        (cond
          ((object? v) (encode-object v))
          ((list? v) (encode-list v))
          ((number? v) (number->string v))
          ((string? v) (encode-string v))
          ((symbol? v) (encode (symbol->string v)))
          ((pair? v) (encode-list v))
          ((eq? v #t) "true")
          ((eq? v #f) "false")
          (else
            (json-syntax-error "unexpeced type: " v) "false"))
        ))
    ))
