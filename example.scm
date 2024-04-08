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

(define (gen b)
  (html/encode
   `(html
     (head
      ((meta (name . "viewport") (content . "width=device-width, initial-scale=1")))
      ((meta (name . "color-scheme") (content . "light dark")))
      ((link (rel . "stylesheet") (href . "https://pub.krzysckh.org/pico.css"))))
     (body
      ,(append
        '((main (class . "container")))
        b)))))

(define (cannot-create why)
  `((code . 200)
    (headers . ((Content-type . "text/html")
                (location . "/")))
    (content . ,(gen
                 `((p "Cannot add user: " ,why " :(")
                   (p "redirecting " ((a (href . "/")) "back") "in <span id=N>3</span> seconds")
                   (script
                    "
const el = document.getElementById('N');
function f(n) {
  el.innerHTML = n;
  if (n == 0)
    window.location.href = '/';
  else
    setTimeout(() => f(n-1), 1000);
}

f(3)"))))))

(define (index-do-post request)
  (let* ((pd (cdr (assq 'post-data request)))
         (uname (cdr (assq 'uname pd)))
         (passwd (cdr (assq 'passwd pd))))
    (cond
     ((not (null? (tsv/filter-by th (λ (u p) (string-ci=? u uname)))))
      (cannot-create "user with that name already exists"))
     ((= (string-length uname) 0)
      (cannot-create "empty username"))
     ((= (string-length passwd) 0)
      (cannot-create "empty passwd"))
     (else
      (tsv/insert-into th (list uname (sha256 passwd)))
      (tsv/save th)
      (robusta/redirect "/")))))

(define (index request)
  (let ((method (cdr (assq 'method request))))
    (cond
     ((eqv? method 'POST)
      (index-do-post request))
     (else
      `((code . 200)
        (headers . ((Content-type . "text/html")))
        (content
         . ,(gen
             `((fieldset
                (legend "Add user")
                ((form (method . "POST"))
                 ((input (type . "text") (name . "uname") (placeholder . "username")))
                 (br)
                 ((input (type . "password") (name . "passwd") (placeholder . "password")))
                 (button "add user")))
               (h3 "users:")
               (table
                ,(append
                  '(tr (th "Username") (th "sha256'd password"))
                  (map (λ (l)
                         (list
                          'tr
                          (append '(td) (list (list-ref l 0)))
                          (append '(td) (list (list-ref l 1)))))
                       (tsv/get-all th))))
               ))))))))

(define static-folder "static/")
(define dispatcher
  (robusta/dispatcher
   `(("/" . ,index)
     ("/internal-server-error" . ,(λ (req) (substring "" 0 100)))
     ("m/.*/" . ,(λ (req) (list
                           '(code . 404)
                           '(headers . ((Content-type . "text/html")))
                           (cons 'content (gen '((h1 "404")
                                                 (p "page not found"))))))))))

(λ (args) (robusta/bind 6969 dispatcher))
