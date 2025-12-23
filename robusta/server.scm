#| doc
serving *stuff*

this library implements a basic http server
|#
(define-library (robusta server)
  (import
   (owl toplevel)
   (owl proof)
   (owl sys)
   (owl thread)
   (robusta common)
   (prefix (robusta http) http/))

  (export
   respond
   response
   bind)

  (begin
    (define a 'a)

    (define-syntax response
      (syntax-rules (=> 42)
        ((_ 42 ff)
         ff)
        ((_ 42 ff key => value . rest)
         (_ 42 (put ff 'key value) . rest))
        ((_ . rest)
         (_ 42 empty . rest))))

    (define (respond r resp)
      ((get r 'send 'bug) resp))

    (define (safe-write-bytes port lst)
      (write-bytes port (if (string? lst) (append (string->list lst) '(10)) lst)))

    (define (c->request ip fd log)
      (let* ((port (fd->port fd))
             (req (http/parse-by-fd fd)))
        (ff
         'send (lambda (resp)
                 (lets ((code    (get resp 'code 200))
                        (headers (get resp 'headers '((Content-type . "text/plain"))))
                        (text    (get resp 'content ""))
                        (siz     (if (string? text)
                                     (string-length text)
                                     (len text))))
                   (safe-write-bytes port (str "HTTP/1.1 " code))
                   (for-each
                    (lambda (v) (safe-write-bytes port (str (car v) ": " (cdr v))))
                    headers)
                   (safe-write-bytes port (str "Content-length: " siz))
                   (safe-write-bytes port "")
                   (safe-write-bytes port text) ;; text may also actually be a list (!!)
                   (log req code siz)
                   (close-port port)))
         'request req
         'ip ip
         'fd fd)))

    (define (bind port f . logger)
      (print "starting server @ http://localhost:" port)
      (let ((sock (open-socket port)))
        (let loop ()
          (lets ((ip fd (tcp-client sock)))
            (thread
             (begin
               (catch-signals (list sigpipe))
               (set-signal-action signal-handler/ignore)
               (let ((r (c->request ip fd (λ (r code siz)
                                            (when (not (null? logger))
                                              ((car logger) r code siz))))))
                 (try-thunk
                  (λ () (f r))
                  (λ vs
                    (respond r (response
                                code    => 500
                                content => (str "500 internal server error: " vs))))
                  (string->symbol (str "try-" (time-ns)))))))
            (loop)))))

    ))
