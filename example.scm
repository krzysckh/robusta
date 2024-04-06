(import (prefix (robusta server) robusta/))
(import (prefix (robusta dispatcher) robusta/))
(import (prefix (robusta encoding html) html/))
(import (only (robusta server) ->string))

(import (prefix (robusta encoding json) json/))

(import (prefix (owl sys) sys/))

(define (index-do-post request)
  (let* ((pd (cdr (assq 'post-data request)))
         (name (cdr (assq 'uname pd)))
         (f (open-output-file "/tmp/last")))
    (print-to f name)
    (sys/close f))
  `((code . 302)
    (headers . ((Content-type . "text/html")
                (location . "/")))
    (content . ,(html/encode
                 `(html
                   (head)
                   (body "302 found"))))))

(define (get-last)
  (if (sys/file? "/tmp/last")
      (car (force-ll (lines (open-input-file "/tmp/last"))))
      "NONE"))

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
                ((link (rel . "stylesheet") (href . "static/pico.css"))))
               (body
                ((main (class . "container"))
                 (p "Last submitted name: "  ,(get-last))
                 ((form (method . "POST"))
                  (label "submit a name: ")
                  ((input (type . "text") (name . "uname")))
                  (button "OK"))

                 ))))))))))

(define static-folder "static/")
(define dispatcher
  (robusta/dispatcher
   `(("/" . ,index)
     ("m/static\\/.*/" . ,(λ (req) (robusta/static-dispatcher static-folder req)))
     ("/static/" . ,(λ (req) (robusta/static-index static-folder req))))))


(λ (args) (robusta/bind 6969 dispatcher))
