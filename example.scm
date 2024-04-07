;; -*- mode: scheme; compile-command: "ol -r example.scm" -*-

(import
 (prefix (robusta server) robusta/)
 (prefix (robusta dispatcher) robusta/)
 (prefix (robusta encoding html) html/)
 (prefix (robusta encoding json) json/)
 (prefix (robusta db tsv) tsv/)
 (prefix (owl sys) sys/)
 (owl digest))

(define (self s) s)

(define schema
  `((uname  ,string? ,self ,self)
    (passwd ,string? ,self ,self)))

(define th (tsv/open "db.tsv" schema))

(define (index-do-post request)
  (let* ((pd (cdr (assq 'post-data request)))
         (uname (cdr (assq 'uname pd)))
         (passwd (cdr (assq 'uname pd))))
    (tsv/insert-into th (list uname (sha256 passwd)))
    (tsv/save th)
    `((code . 302)
      (headers . ((Content-type . "text/html")
                  (location . "/")))
      (content . ,(html/encode
                   `(html
                     (head)
                     (body "302 found")))))))

(define (index request)
  (let ((method (cdr (assq 'method request))))
    (cond
     ((eqv? method 'POST)
      (index-do-post request))
     (else
      `((code . 200)
        (headers . ((Content-type . "text/html")))
        (content
         . ,(html/encode
             `(html
               (head
                ((meta (name . "viewport") (content . "width=device-width, initial-scale=1")))
                ((meta (name . "color-scheme") (content . "light dark")))
                ((link (rel . "stylesheet") (href . "https://pub.krzysckh.org/pico.css"))))
               (body
                ((main (class . "container"))
                 (fieldset
                  (legend "Add user")
                  ((form (method . "POST"))
                   ((input (type . "text") (name . "uname") (placeholder . "username")))
                   (br)
                   ((input (type . "password") (name . "passwd") (placeholder . "password")))
                   (button "add user")))
                 (table
                  ,(append
                    '(tr (th "Username") (th "sha256'd password"))
                    (map (λ (l)
                           (list
                            'tr
                            (append '(td) (list (list-ref l 0)))
                            (append '(td) (list (list-ref l 1)))))
                         (tsv/get-all th))))

                 ))))))))))

(define static-folder "static/")
(define dispatcher
  (robusta/dispatcher
   `(("/" . ,index)
     ("/internal-server-error" . ,(λ (req) (substring "" 0 100))))))

(λ (args) (robusta/bind 6969 dispatcher))
