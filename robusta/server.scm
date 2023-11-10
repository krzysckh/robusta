(define-library
  (robusta server)

  (import
    (owl core)
    (owl io)
    (owl syscall)
    (owl list)
    (owl time)
    (owl lazy)
    (scheme base)
    (prefix (robusta http) http/))

  (export
    bool->string
    ->string     ; i should probably move it somewhere else
    bind)

  (begin
    (define (bool->string v)
      (if v "#t" "#f"))

    (define (->string x)
      (cond
        ((list? x) (foldl string-append
                          ""
                          (map (λ (x) (string-append (->string x) " ")) x)))
        ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        ((boolean? x) (bool->string x))
        ((char? x) (string x))
        ((string? x) x)
        (else
          "")))

    (define (c->request c)
      (let* ((ip (car c))
             (fd (cdr c))
             (port (fd->port fd)))
        ; (send 200 '((Content-type . "text/html")) "<h1> helo </h1>")
        ; you shouldn't provide Content-length
        `((send . ,(lambda (code headers text)
                     (print-to port (->string `("HTTP/1.1" ,code)))
                     (for-each
                       (lambda (v)
                         (print-to port (string-append (->string (car v)) ": "
                                                       (->string (cdr v)))))
                       headers)
                     (print-to port (->string `("Content-length: "
                                                ,(string-length text))))
                     (print-to port "")
                     (print-to port text)))
          (request . ,(http/parse-by-fd fd))
          (ip . ,ip)
          (fd . ,fd))))


    (define (bind port f)
      (letrec*
        ((clients (tcp-clients port))
         (caller (lambda (v)
                   (let ((current (v)))
                     (thread
                       (string->symbol
                         (string-append
                           "thr-"
                           (number->string (time-ns))))
                       (f (c->request (car current)))
                       ) ; this parentheses is spread like that to allow easy
                         ; debugging
                     (caller (cdr current))))))
        (caller clients)))
    ))
