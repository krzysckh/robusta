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
   (only (robusta server) ->string))

  (export
   static-dispatcher
   dispatcher
   file->lst)

  (begin
    (define (dispatch? path req)
      (if (>= (string-length path) 2)
          (if (string=? (substring path 1 2) "/")
              ((string->regex path) req)
              (string=? path req))
          (string=? path req)))

    (define content-type-al
      '((html . "text/html")
        (png  . "image/png")
        (jpg  . "image/jpeg")
        (jpeg . "image/jpeg")
        (exe  . "application/octet-stream")
        (mp4  . "video/mp4")
        (css  . "text/css")
        (js   . "text/javascript")
        (svg  . "image/svg+xml")))

    (define default-content-type "text/plain")

    (define (content-type-by-file f)
      (let* ((ext (last ((string->regex "c/\\./") f) ""))
             (r (assq (string->symbol ext) content-type-al)))
        (if r (cdr r) default-content-type)))

    (define (file->lst f)
      (let* ((p (open-input-file f))
             (r (port->byte-stream p)))
        (force-ll r)))

    (define (static-dispatcher from request)
      (let* ((req-path (cdr (assq 'path request)))
             (fl (string-length from))
             (path (substring req-path fl (string-length req-path))))
        `((code . 200)
          (headers . ((Accept-ranges . "bytes")
                      (Content-type . ,(content-type-by-file path))))
          (content . ,(file->lst (string-append from path))))))

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
