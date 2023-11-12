#| doc
http *stuff*

you probably shouldn't use this library by itself.
it's nicely packaged inside of `(robusta dispatcher)`
|#

(define-library
  (robusta http)

  (import
    (owl core)
    (owl regex)
    (owl io)
    (owl syscall)
    (owl list)
    (owl time)
    (owl lazy)
    (scheme base))

  (export
    parse
    parse-by-fd)

  (begin
    (define (unwrap-request ll acc)
      (cond
        ((pair? ll) (unwrap-request ((cdr ll)) (append acc (list (car ll)))))
        (else
          acc)))

    (define (parse l)
      (let* ((L ((string->regex "c/ /") (car l)))
             (method   (list-ref L 0))
             (path     (list-ref L 1))
             (protocol (list-ref L 2)))
        `((method . ,method)
          (path . ,path)
          (protocol . ,protocol))))

    (define (parse-by-fd fd)
      (parse (unwrap-request ((lines fd)) '())))
    ))
