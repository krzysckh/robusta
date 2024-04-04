#| doc
creating dispatchers

|#
(define-library
  (robusta dispatcher)

  (import
   (owl toplevel)
   (owl regex)
   (owl syscall)
   (owl io)
   (owl list)
   (scheme base)
   (owl sys)
   (robusta mime)
   (prefix (robusta encoding html) html/)
   (only (robusta server) ->string))

  (export
   static-dispatcher
   static-index
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
      (let* ((req-path (cdr (assq 'path request)))
             (fl (string-length from))
             (path (substring req-path fl (string-length req-path)))
             (rpath (string-append from path)))
        (values req-path fl path rpath)))

    (define (static-index from request)
      (lets ((req-path fl path rpath (rpath-vs from request)))
        `((code . 200)
          (headers . ((Content-type . "text/html")))
          (content
           . ,(html/encode
               `(html
                 (head
                  ((meta (charset . "utf-8")))
                  (title ,(string-append "index of " path)))
                 (body
                  (h1 "index of " ,path)
                  ,(append '(ul) (map (λ (e) `(li ((a (href . ,e)) ,e))) (dir->list rpath))))))))))

    (define (static-dispatcher from request)
      (lets ((req-path fl path rpath (rpath-vs from request)))
        (cond
         ((directory? rpath) (static-index from request))
         (else
          `((code . 200)
            (headers . ((Accept-ranges . "bytes")
                        (Content-type . ,(path->mime path))))
            (content . ,(file->lst (string-append from path))))))))

    (define (dispatcher lst)
      (λ (r)
        (let* ((send (cdr (assq 'send r)))
               (request (cdr (assq 'request r)))
               (path (cdr (assq 'path request)))
               (used-dispatcher (car* (filter (λ (d) (dispatch? (car d) path)) lst))))

          ;; TODO: custom 404 page
          (if (null? used-dispatcher)
              (send 404 '((Content-type "text/plain")) "404")
              (begin
                (let* ((vals ((cdr used-dispatcher) request))
                       (code (cdr (assq 'code vals)))
                       (headers (cdr (assq 'headers vals)))
                       (content (cdr (assq 'content vals))))
                  (send code headers content)))))))
    ))
