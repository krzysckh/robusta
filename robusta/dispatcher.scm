(define-library
  (robusta dispatcher)

  (import
    (owl core)
    (owl syscall)
    (owl io)
    (owl list)
    (scheme base))

  (export
    dispatcher)

  (begin
    (define (dispatcher lst)
      (lambda (r)
        (let* ((send (cdr (assq 'send r)))
               (request (cdr (assq 'request r)))
               (path (cdr (assq 'path request)))
               (used-dispatcher (car* (filter
                                        (λ (d) (string=? (car d) path)) lst))))

          ; TODO: custom 404 page
          (if (eqv? '() used-dispatcher)
            (send 404 '((Content-type "text/plain")) "404")
            (begin
              (let* ((vals ((cdr used-dispatcher) request))
                     (code (cdr (assq 'code vals)))
                     (headers (cdr (assq 'headers vals)))
                     (content (cdr (assq 'content vals))))
                (send code headers content)))))))

    ))
