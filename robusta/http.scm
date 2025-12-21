#| doc
http *stuff*

you probably shouldn't use this library by itself.
it's nicely packaged inside of `(robusta dispatcher)`
|#

(define-library
  (robusta http)

  (import
    (owl toplevel)
    (owl regex)
    (prefix (owl sys) sys/)
    (prefix (robusta encoding url) url/)
    (scheme base))

  (export
    parse-by-fd)

  (begin
    (define (unwrap-request ll acc)
      (cond
        ((pair? ll) (unwrap-request ((cdr ll)) (append acc (list (car ll)))))
        (else
          acc)))

    (define cut: (string->regex "c/: ?/"))
    (define fuckbr (string->regex "s/\r//g"))
    (define (get-content-type l)
      (cond
       ((null? l) #f)
       (else
        (let ((vs (cut: (car l))))
          (if (string-ci=? (car vs) "content-type")
              (fuckbr (cadr vs))
              (get-content-type (cdr l)))))))

    ;; TODO: try N times to get a block sized content-length from fd instead of just once
    ;; TODO: right now it's vulnerable to slow lorries
    (define (get-post-data req l bs)
      (if-lets ((hdrs (get req 'headers empty))
                (content-type   (get hdrs 'content-type #f))
                (content-length (get hdrs 'content-length #f)))
        (let ((bytes (force-ll (ltake bs (string->number content-length)))))
          (cond
           ((string=? content-type "application/x-www-form-urlencoded")
            (url/decode-form (list->string bytes)))
           (else
            bytes)))))

    (define (lowercase s)
      (string-map
       (λ (c)
         (if (and (>= c #\A) (<= c #\Z))
             (+ c (- #\a #\A))
             c))
       s))

    (define (lst->headers lst)
      (list->ff
       (map
        (λ (s) (let ((l ((string->regex "c/: */") s)))
                 (cons (string->symbol (lowercase (car l))) (cadr l))))
        lst)))

    (define (bs->line bs)
      (let walk ((bs bs)
                 (acc #n))
        (cond
         ((function? bs) (walk (bs) acc))
         ((pair? bs)
          (cond
           ((eq? (car bs) #\return)
            (walk (cdr bs) acc))
           ((eq? (car bs) #\newline)
            (values (cdr bs) (reverse acc)))
           (else
            (walk (cdr bs) (cons (car bs) acc)))))
         (else
          acc))))
          ;; (error "no clue what is " bs)))))

    (define (fd->request-lines fd)
      (let walk ((bs (port->byte-stream fd))
                 (ls #n))
        (lets ((bs l (bs->line bs)))
          (if (null? l)
              (values bs (reverse ls))
              (walk bs (cons (list->string l) ls))))))

    (define (parse-by-fd fd)
      (lets ((bs l (fd->request-lines fd))
             (L ((string->regex "c/ /") (fuckbr (car l))))
             (method   (string->symbol (lref L 0)))
             (path     (lref L 1))
             (protocol (lref L 2)))
        (let ((res (ff
                    'headers  (lst->headers (cdr l))
                    'method   method
                    'path     path
                    'protocol protocol)))
          (if (eqv? method 'POST)
              (put res 'post-data (get-post-data res l bs))
              res))))
    ))
