(import (prefix (robusta server) robusta/))
(import (prefix (robusta dispatcher) robusta/))

(define (index request)
  '((code . 200)
    (headers . ((Content-type . "text/html")))
    (content . "this is the index page")))

(define (about request)
  '((code . 200)
    (headers . ((Content-type . "text/html")))
    (content . "this is an about page")))

(define dispatcher (robusta/dispatcher
                     `(("/" . ,index)
                       ("/about" . ,about))))

(robusta/bind 8080 dispatcher)
