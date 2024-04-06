#| doc
http *stuff*

you probably shouldn't use this library by itself.
it's nicely packaged inside of `(robusta dispatcher)`
|#

(define-library
  (robusta http)

  (import
    (owl toplevel)
    (owl regex)
    (owl io)
    (owl syscall)
    (owl time)
    (owl lazy)
    (prefix (owl sys) sys/)
    (prefix (robusta encoding url) url/)
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

    (define cut: (string->regex "c/: ?/"))
    (define fuckbr (string->regex "s/\r//g"))
    (define (get-content-type l)
      (cond
       ((null? l) #f)
       (else
        (let ((vs (cut: (car l))))
          (if (string-ci=? (car vs) "content-type")
              (fuckbr (cadr vs))
              (get-content-type (cdr l)))))))

    (define (get-raw-post-data l)
      (if (null? l)
          ""
          (let ((cur (fuckbr (car l))))
            (cond
             ((string=? cur "")
              (foldl string-append "" (cdr l)))
             (else
              (get-raw-post-data (cdr l)))))))

    (define (get-post-data l)
      (let ((content-type (get-content-type l))
            (data (get-raw-post-data l)))
        (cond
         ((string=? content-type "application/x-www-form-urlencoded")
          (let ((things ((string->regex "c/&/") data)))
            (map (λ (s)
                   (let* ((vs ((string->regex "c/=/") s))
                          (k (car vs))
                          (v (cadr vs)))
                     (cons k (url/decode v))))
                 things)))
         (else
          `(error . ,(string-append "content-type " content-type "unsupported"))))))


    (define (parse l)
      (let* ((L ((string->regex "c/ /") (car l)))
             (method   (string->symbol (list-ref L 0)))
             (path     (list-ref L 1))
             (protocol (list-ref L 2)))
        (if (eqv? method 'POST)
            `((method . ,method)
              (path . ,path)
              (protocol . ,protocol)
              (post-data . ,(get-post-data l)))
            `((method . ,method)
              (path . ,path)
              (protocol . ,protocol)))))

    (define READ-MAX (<< 2 30)) ;; this sucks but is sane ig
    (define (parse-by-fd fd)
      (parse ((string->regex "c/\n/") (list->string (bytevector->list (sys/read fd READ-MAX))))))
    ))
