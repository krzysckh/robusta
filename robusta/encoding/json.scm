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

    ;; s "" → string(n)
    (define (get-number s acc)
      (cond
        ((digit? s)
         (get-number (substring s 1 (string-length s))
                     (string-append acc (substring s 0 1))))
        (else
          acc)))

    ;; s "" -> (parsed-s . skip)
    (define (get-string s acc)
      (error "not implemented" "")
      )

    (define (get-array-skip s acc)
      (cond
        ((string=? (substring s 0 1) "]") (+ acc 1))
        (else
          (get-array-skip (substring s 1 (string-length s)) (+ 1 acc)))))

    (define (parse-array s acc)
      (let ((first-char (substring s 0 1)))
        (cond
          ((string=? first-char "]") acc)
          ((or (string=? first-char " ")
               (string=? first-char "\t")
               (string=? first-char ","))
           (parse-array (substring s 1 (string-length s)) acc))
          (((string->regex "m/^[0-9]+/") s)
           (let ((n (get-number s "")))
             (parse-array
               (substring s (string-length n) (string-length s))
               (append acc `(,(string->number n))))))
          ((string=? first-char "[")
           (parse-array
             (substring s (get-array-skip s 0) (string-length s))
             (append acc
                     (list (parse-array
                             (substring s 1 (string-length s))
                             '())))))
          ((string=? first-char "\"")
           (let* ((S (get-string s ""))
                  (actual-string (car S))
                  (skip (cdr S)))
             (parse-array (substring s skip (string-length s))
                          (append acc `(,actual-string)))))
          (else
            (print "not-implemented: true | false | object")
            (json-syntax-error "failed to parse-array"
                               (string-append "here → "s))))))

    (define (parse-object s acc)
      (error "TODO" "")
      )

    (define (decode s)
      (let ((first-char (car* (string->list s))))
        (cond
          ((eq? first-char #\[)
           (parse-array (substring s 1 (string-length s)) '()))
          ((eq? first-char #\{)
           (parse-object (substring s 1 (string-length s)) '()))
          (else
            (json-syntax-error "invalid first-char: "
                   (string-append "here → " s)
                   "expected: [ or {")))))

    (define (encode lst)
      (error "not" 'implemented) ; fancy
      )
    ))
