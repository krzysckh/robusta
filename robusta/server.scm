#| doc
serving *stuff*

this library implements a basic http server
|#
(define-library
  (robusta server)

  (import
    (owl toplevel)
    (owl proof)
    (owl sys)
    (owl syscall)
    (owl thread)
    (owl lazy)
    (robusta common)
    (prefix (robusta http) http/))

  (export
    bind)

  (begin
    (define (safe-write-bytes port lst)
      (if (writeable? port)
        (write-bytes port (if (string? lst) (append (string->list lst) '(10)) lst))
        (print "port not writeable")))

    (define (c->request c)
      (let* ((ip (car c))
             (fd (cdr c))
             (port (fd->port fd)))
        ;; TODO: find out why owl fails somewhere here with errno SIGPIPE from
        ;; send(), although, i'm clearly checking if port is writeable?
        ;; i really, really don't want to check that on each and every print-to
        ;; call, or maybe i should define it as a function? i don't know.
        ;; i am clearly not out of ideas, yet i have no internal need to
        ;; implement and fix that now, though i know this is a serious problem,
        ;; and i should repair that immidiately. my mind really just wants to
        ;; relax and write (robusta encoding json) :3

        ;; (send 200 '((Content-type . "text/html")) "<h1> helo </h1>")
        ;; you shouldn't provide Content-length
        `((send . ,(lambda (code headers text)
                     (safe-write-bytes port (->string `("HTTP/1.1" ,code)))
                     (for-each
                       (lambda (v)
                         (safe-write-bytes port (string-append
                                               (->string (car v))
                                               ": " (->string (cdr v)))))
                       headers)
                     (safe-write-bytes port (->string `("Content-length: "
                                                     ,(if (string? text)
                                                          (string-length text)
                                                          (length text)))))
                     (safe-write-bytes port "")
                     (safe-write-bytes port text) ;; text may also actually be a list (!!)
                     (close-port port)))
          (request . ,(http/parse-by-fd fd))
          (ip . ,ip)
          (fd . ,fd))))


    (define (bind port f)
      (print "starting server @ http://localhost:" port)
      (letrec*
        ((clients (tcp-clients port))
         (caller (lambda (v)
                   (let ((current (v)))
                     ;; (print (list "in caller: " current))
                     (thread
                       (string->symbol
                         (string-append
                           "thr-"
                           (number->string (time-ns))))
                       (begin
                         (catch-signals (list sigpipe))
                         (set-signal-action signal-handler/ignore)
                         (let ((r (c->request (car current))))
                           ((try (λ () (f r) (λ () 0))
                                 (λ () (let ((s (cdr (assq 'send r))))
                                         (s 501 '((Content-type . "text/html")) "501 internal server error")))))))
                       ) ;; this parentheses is spread like that to allow easy debugging
                     (caller (cdr current))))))
        (caller clients)))

    ))
