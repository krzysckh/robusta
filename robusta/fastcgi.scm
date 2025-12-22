#| doc
fastcgi server

safer and faster way to host robusta web applications.

Example configurations, assuming web app running at 127.0.0.1:8080:

- apache2

```
<VirtualHost *:80>
  DocumentRoot /var/www/html

  ProxyPassMatch ^/(.*)$ fcgi://127.0.0.1:8080/

  <Directory />
    Require all granted
  </Directory>
</VirtualHost>
```
|#

(define-library
    (robusta fastcgi)

  (import
   (owl toplevel)
   (owl proof)
   (owl thread)
   (owl sys)
   (only (robusta http) maybe-parse-post-data)
   (robusta common))

  (export
   fastcgi-bind)

  (begin
    (define FCGI-BEGIN-REQUEST       1)
    (define FCGI-ABORT-REQUEST       2)
    (define FCGI-END-REQUEST         3)
    (define FCGI-PARAMS              4)
    (define FCGI-STDIN               5)
    (define FCGI-STDOUT              6)
    (define FCGI-STDERR              7)
    (define FCGI-DATA                8)
    (define FCGI-GET-VALUES          9)
    (define FCGI-GET-VALUES-RESULT  10)
    (define FCGI-UNKNOWN-TYPE       11)
    (define FCGI-MAXTYPE FCGI-UNKNOWN-TYPE)

    ;; Ignored for now. Always keeps a connection.
    (define FCGI-KEEP-CONN  1)

    ;; Ignored for now
    (define FCGI-RESPONDER  1)
    (define FCGI-AUTHORIZER 2)
    (define FCGI-FILTER     3)

    ;; Ignored for now
    (define FCGI-REQUEST-COMPLETE 0)
    (define FCGI-CANT-MPX-CONN    1)
    (define FCGI-OVERLOADED       2)
    (define FCGI-UNKNOWN-ROLE     3)


    (define (fastcgi-get-frame fd)
      (lets ((hdr (try-get-block fd 8 #t)))
        (if (eof-object? hdr)
            #f
            (lets ((version (bytevector-u8-ref hdr 0))
                   (type (bytevector-u8-ref hdr 1))
                   (id (+ (<< (bytevector-u8-ref hdr 2) 8)
                          (bytevector-u8-ref hdr 3)))
                   (length (+ (<< (bytevector-u8-ref hdr 4) 8)
                              (bytevector-u8-ref hdr 5)))
                   (padl (bytevector-u8-ref hdr 6)))
              (ff
               'id id
               'type type
               'data (try-get-block fd length #t)
               'pad (try-get-block fd padl #t))))))

                                        ; data -> data' (k . v)
    (define (fastcgi-get-kv lst)
      (lets ((nlen  lst (values (car lst) (cdr lst)))
             (vlen1 lst (values (car lst) (cdr lst)))
             (vlenr lst (if (= (>> vlen1 7) 0)
                            (values #f lst)
                            (values (take lst 3) (drop lst 3))))
             (vlen (if vlenr
                       (+ (band (<< vlen1 24) #x7f)
                          (<< (lref vlenr 0) 16)
                          (<< (lref vlenr 1) 8)
                          (lref vlenr 2))
                       vlen1))
             (key lst (values (take lst nlen) (drop lst nlen)))
             (val lst (values (take lst vlen) (drop lst vlen))))
        (values lst (cons (bytes->string key) (bytes->string val)))))

    (define (fastcgi-build-frame type id data)
      `(1                       ; version
        ,type                   ; type
        ,(>> id 8)              ; \
        ,(band id #xff)         ; / id
        ,(>> (len data) 8)      ; \
        ,(band (len data) #xff) ; / content length
        0 0
        ,@data))                ; data

    (define (fastcgi-send-response fd id data)
      (lets ((d rest (values (take data #xffff) (drop data #xffff))))
        (let ((frame (fastcgi-build-frame FCGI-STDOUT id d)))
          (write-bytes fd frame)
          (if (not (null? rest))
              (fastcgi-send-response fd id rest)
              (begin
                (write-bytes fd (fastcgi-build-frame FCGI-STDOUT id '()))
                (write-bytes fd (fastcgi-build-frame FCGI-END-REQUEST id '(0 0 0 0 0 0 0 0))))))))

    (define (kvize c)
      (str (car c) ": " (cdr c)))

    (define (fastcgi-make-headers h)
      (string-append
       (fold (λ (a b) (string-append a "\r\n" (kvize b))) (kvize (car h)) (cdr h))
       "\r\n\r\n"))

    (define (fastcgi-fire-request f fd id data logger)
      (lets ((params (get data 'params #n))
             (headers
              (put
               (list->ff
                (map
                 (λ (c) (cons (string->symbol (lowercase ((string->regex "s/_/-/g")
                                                          ((string->regex "s/^HTTP_//") (car c)))))
                              (cdr c)))
                 (filter (B (string->regex "m/^HTTP_/") car) params)))
               'content-type
               (or (cdr* (assoc "CONTENT_TYPE" params)) "unknown")))
             (request (ff
                       'headers   headers
                       'method    (string->symbol (cdr (assoc "REQUEST_METHOD" params)))
                       'path      (cdr (assoc "REQUEST_URI" params))
                       'protocol  (cdr (assoc "SERVER_PROTOCOL" params))
                       'post-data (maybe-parse-post-data
                                   (get data 'stdin #n)
                                   (get headers 'content-type "[unknown]"))
                       )))
        (f (ff
            'send (λ (resp)
                    (lets ((code    (get resp 'code 200))
                           (headers (get resp 'headers '((Content-type . "text/plain"))))
                           (text    (get resp 'content ""))
                           (siz     (if (string? text)
                                        (string-length text)
                                        (len text)))
                           (response-body (if (list? text) text (string->bytes text))))
                      (logger request code (len response-body))
                      (fastcgi-send-response
                       fd id
                       (append
                        (string->bytes (fastcgi-make-headers (cons (cons 'Status code) headers)))
                        response-body))))
            'request request
            'ip (bytevector 0 0 0 0)
            'fd #f))))

    (define (fastcgi-bind port f . logger)
      (print "starting server @ http://localhost:" port)
      (let ((sock (open-socket port)))
        (let loop ()
          (lets ((ip fd (tcp-client sock)))
            ;; (thread
             (begin

               (catch-signals (list sigpipe))
               (set-signal-action signal-handler/ignore)

               ;; rdata = ff of id -> ff of (  params -> ( ... )
               ;;                            , stdin  -> ( ... )
               ;;                            )
               (let walk ((rdata empty))
                 (lets ((frame (fastcgi-get-frame fd)))
                   (when frame
                     (lets ((id (get frame 'id -1))
                            (data (get frame 'data #n)))
                       (case (get frame 'type 'bug)
                         (FCGI-PARAMS
                          (if (eof-object? data)
                              (walk rdata)
                              (walk
                               (put
                                rdata id
                                (put
                                 (get rdata id empty)
                                 'params
                                 (append
                                  (let ploop ((lst (bytevector->list data)))
                                    (if (null? lst)
                                        #n
                                        (lets ((lst kv (fastcgi-get-kv lst)))
                                          (cons kv (ploop lst)))))
                                  (get (get rdata id empty) 'params #n)))))))
                         (FCGI-STDIN
                          (if (eof-object? data)
                              (let ((ob (get rdata id empty)))
                                (fastcgi-fire-request f fd id ob (λ (r code siz)
                                                                   (when (not (null? logger))
                                                                     ((car logger) r code siz))))
                                (walk (put rdata id empty)))
                              (walk
                               (put
                                rdata id
                                (put
                                 (get rdata id empty)
                                 'stdin
                                 (append
                                  (get (get rdata id empty) 'stdin #n)
                                  (bytevector->list data)))))))
                         (else => (λ (fr)
                                    ;; (print "ignoring frame " fr)
                                    (walk rdata))))))))
               (close-port fd)))
          ;; )
          (loop))))
    ))
