(import (prefix (robusta server) robusta/))
(import (prefix (robusta dispatcher) robusta/))
(import (prefix (robusta encoding html) html/))
(import (only (robusta server) ->string))

(import (prefix (robusta encoding json) json/))

(define (index request)
  `((code . 200)
    (headers . ((Content-type . "text/html")))
    (content . ,(html/encode `(html (head) (body (p ,(->string request))))))))

(define static-folder "static/")
(define dispatcher
  (robusta/dispatcher
   `(("m/^\\/(index)?/" . ,index)
     ("m/static\\/.*/" . ,(λ (req) (robusta/static-dispatcher static-folder req)))
     ("/static/" . ,(λ (req) (robusta/static-index static-folder req))))))


(λ (args) (robusta/bind 6969 dispatcher))
