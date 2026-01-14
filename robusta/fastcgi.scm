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

- OpenBSD httpd

```
server "robusta.local" {
  listen on 0.0.0.0 port 80
  log style forwarded
  location "*" {
    fastcgi socket tcp 127.0.0.1 8080
  }
}
```

|#

(define-library
    (robusta fastcgi)

  (import
   (owl toplevel)
   (owl proof)
   (owl thread)
   (owl sys)
   (only (owl eval env) verbose-vm-error)
   (only (robusta http) maybe-parse-post-data)
   (robusta server)
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

    (define (get-whole-block fd size)
      (if (= size 0)
          (eof-object)
          (let loop ((bv (bytevector))
                     (remaining size))
            (if (= size (bytevector-length bv))
                bv
                (let ((new (try-get-block fd remaining #t)))
                  (if (eof-object? new)
                      (eof-object)
                      (loop (bytevector-append bv new) (- remaining (bytevector-length new)))))))))

    (define (fastcgi-get-frame fd)
      (lets ((hdr (get-whole-block fd 8)))
        (if (eof-object? hdr)
            #f
            (lets ((version (bytevector-u8-ref hdr 0))
                   (type (bytevector-u8-ref hdr 1))
                   (id (+ (<< (bytevector-u8-ref hdr 2) 8)
                          (bytevector-u8-ref hdr 3)))
                   (length (+ (<< (bytevector-u8-ref hdr 4) 8)
                              (bytevector-u8-ref hdr 5)))
                   (padl (bytevector-u8-ref hdr 6))
                   (data (get-whole-block fd length))
                   (pad  (get-whole-block fd padl)))
              (ff
               'id id
               'type type
               'data data
               'pad pad)))))

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
      (bytevector-append
       (bytevector
        1                                     ; version
        type                                  ; type
        (>> id 8)                             ; \
        (band id #xff)                        ; / id
        (>> (bytevector-length data) 8)       ; \
        (band (bytevector-length data) #xff)  ; / content length
        0 0)
       data))                                 ; data

    (define (data->bv data)
      (cond
       ((list? data)       (list->bytevector data))
       ((bytevector? data) data)
       ((string? data)     (list->bytevector (string->bytes data)))
       ((function? data)   data)
       (else               (error "unknown type for data->list " data))))

    ;; send data, in as many frames as needed
    ;; does NOT end the request
    (define (fastcgi-send fd id data)
      (let loop ((top 0))
        (if (>= top (bytevector-length data))
            #t
            (let ((end (min (+ top #xffff) (bytevector-length data))))
              (write-bytevector
               (fastcgi-build-frame
                FCGI-STDOUT
                id
                (bytevector-copy data top end))
               fd)
              (loop end)))))

    (define (fastcgi-end-request fd id)
      (write-bytevector (fastcgi-build-frame FCGI-STDOUT id (bytevector)) fd)
      (write-bytevector (fastcgi-build-frame FCGI-END-REQUEST id (bytevector 0 0 0 0 0 0 0 0)) fd))

    (define (fastcgi-send-response fd id data)
      (fastcgi-send fd id data)
      (fastcgi-end-request fd id))

    ;; TODO: streaming here is very CPU intensive, i think it's because bytevector operations
    ;; do bv->list->bv conversions for bytevector-copy. this is less than ideal.
    (define (fastcgi-stream-response fd id headers-string fn)
      (fastcgi-send fd id headers-string)
      (let ((stream (λ (data) (fastcgi-send fd id (data->bv data)))))
        (fn stream)
        (fastcgi-end-request fd id)))

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
                                   (reverse (get data 'stdin #n))
                                   (get headers 'content-type "[unknown]"))
                       ))
             (req (ff
                   'send (λ (resp)
                           (lets ((code    (get resp 'code 200))
                                  (headers (get resp 'headers '((Content-type . "text/plain"))))
                                  (text    (get resp 'content ""))
                                  (siz     (content-length text))
                                  (response-body (data->bv text))
                                  (headers (data->bv (fastcgi-make-headers (cons (cons 'Status code) headers)))))
                             ; (print "response-body: " response-body)
                             (logger request code siz)
                             (if (function? response-body)
                                 (fastcgi-stream-response fd id headers response-body)
                                 (fastcgi-send-response fd id (bytevector-append headers response-body)))))
                   'request request
                   'ip (bytevector 0 0 0 0)
                   'fd #f)))
        (try-thunk
         (λ () (f req))
         (λ vs                          ; TODO: move this outside and use with regular server as well
           (respond req (response
                         code    => 500
                         content => (str "500 internal server error: "
                                         (if (= (len vs) 1)
                                             (verbose-vm-error
                                              empty
                                              (ref (car vs) 2)
                                              (ref (car vs) 3)
                                              (ref (car vs) 4))
                                             vs)))))
         (string->symbol (str "try-" (time-ns))))))

    (define (fastcgi-bind port f . logger)
      (print "starting server @ http://localhost:" port)
      (let ((sock (open-socket port)))
        (let loop ()
          (lets ((ip fd (tcp-client sock)))
            (thread
             (begin

               ;; TODO: an early closed connection on streamed data can cause a sigpipe death loop
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
                                 (cons
                                  data
                                  (get (get rdata id empty) 'stdin #n)))))))
                         (else => (λ (fr)
                                    ;; (print "ignoring frame " fr)
                                    (walk rdata))))))))
               (close-port fd)))
          )
          (loop))))
    ))
