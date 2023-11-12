#| doc
{encoding,decoding} json

this loosely implements the ECMA-404 standard for json encoding and decoding

it WILL accept broken json, and decode it correctly or incorrectly
depending on the mood, but it *should* decode *proper* json correctly.

features, not bugs, include, but are not limited to:
  - `[1,2,3 4]` → `(1 2 3 4)`
  - `[,,,,,]`   → `()`

https://ecma-international.org/publications-and-standards/standards/ecma-404/
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
    object?
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


    (define (object? lst)
          (not (has? (map (λ (l) (or (pair? l)
                                     (and (list? l)
                                          (eq? (length l) 2)
                                          (not (list? (car l)))))) lst) #f)))
    (define (digit? chr)
      (let ((c (cond
                 ((string? chr) (substring chr 0 1))
                 ((char? chr) (string chr))
                 ((number? chr) (number->string chr))
                 (else
                   (error "digit?" "unexpected type")))))
        ((string->regex "m/^[0-9]/") c)))

    ; FIXME: do it as ECMA-404's fig. 4
    ;; s "" → string(n)
    (define (get-number s acc)
      (cond
        ((digit? s)
         (get-number (substring s 1 (string-length s))
                     (string-append acc (substring s 0 1))))
        (else
          acc)))

    ;; s "" → (parsed-s . skip)
    (define (get-string s acc)
      (error "not implemented" ""))

    ;; s "" → (string(tok) | #f . skip | #f)
    (define (get-literal-tok s acc)
      (let ((first-char (car (string->list s))))
        (cond
          ((or (eqv? first-char #\])
               (eqv? first-char #\})
               (eqv? first-char #\,)
               (eqv? first-char #\space))
           (if (or (string=? acc "true")
                   (string=? acc "false")
                   (string=? acc "null"))
             `(,acc . ,(string-length acc))
             `(#f . #f)))
          (else
            (get-literal-tok (substring s 1 (string-length s))
                             (string-append acc (substring s 0 1)))))))

    (define (get-array-skip s)
      (letrec ((f (lambda (s acc bad-acc)
                    (cond
                      ((and (string=? (substring s 0 1) "]")
                            (<= bad-acc 1)) (+ acc 1))
                      ((string=? (substring s 0 1) "]")
                       (f (substring s 1 (string-length s))
                          (+ 1 acc)
                          (- bad-acc 1)))
                      ((string=? (substring s 0 1) "[")
                       (f (substring s 1 (string-length s))
                          (+ 1 acc)
                          (+ 1 bad-acc)))
                      (else
                        (f (substring s 1 (string-length s))
                           (+ 1 acc)
                           bad-acc))))))
        (f s 0 0)))

    (define (next-token s)
      (let ((first-char (car (string->list s)))
            (n-re (string->regex "m/^[0-9]+/")))
        (cond
          ((n-re s)
           (substring s (string-length (get-number s "")) (string-length s)))
          ((eq? first-char #\")
           (substring s (cdr (get-string s)) (string-length s)))
          ((eq? first-char #\space)
           (next-token (substring s 1 (string-length s))))
          ((car (get-literal-tok s ""))
           (substring s (string-length (car (get-literal-tok s "")))
                      (string-length s)))
          (else
            (substring s 1 (string-length s))))))

    (define (parse-object s acc)
      (error "TODO" "")
      )

    (define (decode s)
      (letrec ((parse-array
                 (lambda (s acc)
                   (let ((first-char (substring s 0 1)))
                     (cond
                       ((string=? first-char "]") acc)
                       ((string=? first-char "[")
                         (parse-array (substring s (get-array-skip s)
                                                 (string-length s))
                                      (append acc (list (decode s)))))
                       ((or (string=? first-char " ")
                            (string=? first-char "\t")
                            (string=? first-char ","))
                        (parse-array (substring s 1 (string-length s)) acc))
                       (else
                         (parse-array (next-token s)
                                      (append acc (list (decode s)))))))))
               (first-char (car* (string->list s))))
        (cond
          ((eq? first-char " ") (decode (substring s 1 (string-length s))))
          ((digit? first-char) (string->number (get-number s "")))
          ((eq? first-char "\"") (car (get-string s "")))
          ((eq? first-char #\[)
           (parse-array (substring s 1 (string-length s)) '()))
          ((eq? first-char #\{)
           (parse-object (substring s 1 (string-length s)) '()))
          ((car (get-literal-tok s ""))
           (let* ((T (get-literal-tok s ""))
                  (tok (car T))
                  (skip (cdr T)))
             (cond
               ((string=? "true" tok) #t)
               ((string=? "false" tok) #f)
               ((string=? "null" tok) 'null))))
          (else
            (json-syntax-error "invalid char:"
                   (string-append "here → " s)
                   "expected: [ | { | [0-9]+ | \" | true | false | null")))))

    (define (encode lst)
      (error "not" 'implemented) ; fancy
      )
    ))
