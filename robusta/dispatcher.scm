#| doc
creating dispatchers

|#
(define-library
  (robusta dispatcher)

  (import
    (owl core)
    (owl regex)
    (owl syscall)
    (owl io)
    (owl list)
    (scheme base))

  (export
    dispatcher)

  (begin
    (define (dispatch? path req)
      (if (>= (string-length path) 2)
        (if (string=? (substring path 1 2) "/")
          ((string->regex path) req)
          (string=? path req))
        (string=? path req)))


    (define (dispatcher lst)
      (lambda (r)
        (let* ((send (cdr (assq 'send r)))
               (request (cdr (assq 'request r)))
               (path (cdr (assq 'path request)))
               (used-dispatcher (car* (filter
                                        (λ (d) (dispatch? (car d) path)) lst))))

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
