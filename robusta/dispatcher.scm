#| doc
creating dispatchers

|#
(define-library
  (robusta dispatcher)

  (import
   (owl toplevel)
   (owl regex)
   (owl sys)
   (robusta mime)
   (robusta server)
   (prefix (robusta encoding html) html/)
   (prefix (robusta encoding base64) b64/)
   (only (robusta common) ->string))

  (export
   make-dispatcher
   static-dispatcher
   static-index
   redirect
   dispatcher
   with-http-basic-auth
   )

  (begin
    (define (dispatch? path req)
      (if (>= (string-length path) 2)
          (if (string=? (substring path 1 2) "/")
              ((string->regex path) req)
              (string=? path req))
          (string=? path req)))

    ;; TODO: remove unneeded // -> /
    ;; can be bodged with s/\\/\\//\\/g OR done properly with some parsing
    (define (static-index path ipath)
      (response
       code    => 200
       headers => '((Content-type . "text/html"))
       content => (html/encode
                   `(html
                     (head
                      ((meta (charset . "utf-8")))
                      (title "index of " ,ipath))
                     (body
                      (h1 "index of " ,ipath)
                      ,(append '(ul) (map (λ (e) `(li ((a (href . ,(str ipath "/" e))) ,e))) (dir->list path))))))))

    (define (rpath-of from base request)
      (let ((rp (get request 'path 'bug)))
        (string-append
         from
         (substring rp (string-length base) (string-length rp)))))

    (define (static-dispatcher from base request)
      (lets ((rpath (rpath-of from base request)))
        (cond
         ((directory? rpath) (static-index rpath (get request 'path "")))
         ((file? rpath)
          (response
           code    => 200
           headers => `((Accept-ranges . "bytes") (Content-type . ,(path->mime rpath)))
           content => (file->list rpath)))
         (else
          (response
           code    => 404
           headers => `((Content-type . "text/html"))
           content => "404 not found")))))

    (define (dispatcher lst)
      (λ (r)
        (lets ((path (get (get r 'request empty) 'path ""))
               (used-dispatcher (car* (filter (λ (d) (dispatch? (car d) path)) lst))))
          ;; TODO: custom 404 page
          (if (null? used-dispatcher)
              (respond r (response
                          code => 404
                          content => "404"))
              (respond r ((cdr used-dispatcher) (get r 'request empty))))
          )))

    (define-syntax make-dispatcher
      (syntax-rules (=> 42)
        ((_ 42)
         #n)
        ((_ 42 k => v . rest)
         (cons (cons k v) (_ 42 . rest)))
        ((_ . rest)
         (dispatcher (_ 42 . rest)))))

    (define (redirect path . code)
      (response
       code => (if (null? code) 302 (car code))
       headers => `((Content-type . "text/html") (location . ,path))
       content => (html/encode
                   `(html
                     (head)
                     (body "302 found")))))

    (define (with-http-basic-auth* req username passwd thunk fail-fn)
      (lets ((ok (str "Basic " (b64/string->base64 (format #f "~a:~a" username passwd))))
             (auth (get (get req 'headers empty) 'authorization #f)))
        (if (equal? ok auth)
            (thunk)
            (fail-fn))))

    (define-syntax with-http-basic-auth
      (syntax-rules (code content headers Content-type WWW-Authenticate)
        ((_ (req username passwd) code* code-else)
         (with-http-basic-auth* req username passwd (λ () code*) (λ () code-else)))
        ((_ (req username passwd) code*)
         (with-http-basic-auth* req username passwd
           (λ () code*)
           (λ () (ff
                  'code    401
                  'content "unauthorized"
                  'headers '((Content-type . "text/html")
                             (WWW-Authenticate . "Basic realm=\"authenticate\""))))))))

))
