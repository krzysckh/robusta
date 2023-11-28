(import (prefix (robusta server) robusta/))
(import (prefix (robusta dispatcher) robusta/))
(import (only (robusta server) ->string))

(import (prefix (robusta encoding json) json/))

(define (index request)
  '((code . 200)
    (headers . ((Content-type . "text/html")))
    (content . "this is the index page")))

(define (about request)
  `((code . 200)
    (headers . ((Content-type . "text/html")))
    (content . ,(->string
                  (list "this is an about page<br>"
                        "your request: " request)))))

(define dispatcher
  (robusta/dispatcher
    `(("/" . ,index)
      ("/about" . ,about)
      ("/api" . ,(lambda (req)
                   `((code . 200)
                     (headers . ((Content-type "application/json")))
                     (content . ,(json/encode '((a . "10") (b .  #f))))))))))

(robusta/bind 8080 dispatcher)
