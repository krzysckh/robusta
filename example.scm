(import (prefix (robusta server) robusta/))
(import (prefix (robusta dispatcher) robusta/))
(import (only (robusta server) ->string))

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

(define dispatcher (robusta/dispatcher
                     `(("/" . ,index)
                       ("/about" . ,about))))

(robusta/bind 8080 dispatcher)
