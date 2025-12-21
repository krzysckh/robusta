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
   (only (robusta common) ->string))

  (export
   make-dispatcher
   static-dispatcher
   static-index
   redirect
   dispatcher
   file->lst)

  (begin
    (define (dispatch? path req)
      (if (>= (string-length path) 2)
          (if (string=? (substring path 1 2) "/")
              ((string->regex path) req)
              (string=? path req))
          (string=? path req)))

    (define (file->lst f)
      (let* ((p (open-input-file f))
             (r (port->byte-stream p)))
        (force-ll r)))

    (define (rpath-vs from request)
      (let* ((req-path (get request 'path 'bug))
             (fl (string-length from))
             (path (substring req-path fl (string-length req-path)))
             (rpath (string-append from path)))
        (values req-path fl path rpath)))

    (define (static-index from request)
      (lets ((req-path fl path rpath (rpath-vs from request)))
        (response
         code    => 200
         headers => '((Content-type . "text/html"))
         content => (html/encode
                     `(html
                       (head
                        ((meta (charset . "utf-8")))
                        (title ,(string-append "index of " path)))
                       (body
                        (h1 "index of " ,path)
                        ,(append '(ul) (map (λ (e) `(li ((a (href . ,e)) ,e))) (dir->list rpath)))))))))

    (define (static-dispatcher from request)
      (lets ((req-path fl path rpath (rpath-vs from request)))
        (cond
         ((directory? rpath) (static-index from request))
         (else
          (response
           code    => 200
           headers => `((Accept-ranges . "bytes") (Content-type . ,(path->mime path)))
           content => (file->lst (string-append from path)))))))

    ;; TODO: fuck! path traversal
    (define (dispatcher lst)
      (λ (r)
        (lets ((path (get (get r 'request empty) 'path #f))
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
))
