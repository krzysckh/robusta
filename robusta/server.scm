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

    (define (c->request ip fd)
      (let* ((port (fd->port fd)))
        ;; (send 200 '((Content-type . "text/html")) "<h1> helo </h1>")
        ;; you shouldn't provide Content-length
        (ff
         'send (lambda (resp)
                 (lets ((code    (get resp 'code 200))
                        (headers (get resp 'headers '((Content-type . "text/plain"))))
                        (text    (get resp 'content "")))
                   (safe-write-bytes port (str "HTTP/1.1 " code))
                   (for-each
                    (lambda (v) (safe-write-bytes port (str (car v) ": " (cdr v))))
                    headers)
                   (safe-write-bytes port (str "Content-length: " (if (string? text)
                                                                      (string-length text)
                                                                      (len text))))
                   (safe-write-bytes port "")
                   (safe-write-bytes port text) ;; text may also actually be a list (!!)
                   (close-port port)))
         'request (http/parse-by-fd fd)
         'ip ip
         'fd fd)))

    ;; TODO: should i return info about internal server errors? they're not really descriptive so
    ;; i think that might be okay
    (define (bind port f)
      (print "starting server @ http://localhost:" port)
      (let ((sock (open-socket port)))
        (let loop ()
          (lets ((ip fd (tcp-client sock)))
            (thread
             (begin
               (catch-signals (list sigpipe))
               (set-signal-action signal-handler/ignore)
               (try-thunk
                (λ ()
                  (let ((r (c->request ip fd)))
                    (try-thunk
                     (λ () (f r))
                     (λ vs
                       (respond r (response
                                   code    => 501
                                   content => (str "501 internal server error: " vs))))
                     (string->symbol (str "try-" (time-ns))))))
                (λ vs
                  (safe-write-bytes fd (str "HTTP/1.1 400\r\nContent-type: text/plain\r\n\r\nbad request: " vs))
                  (close-port fd))
                (string->symbol (str "try-" (time-ns)))))))
          (loop))))

    ))
