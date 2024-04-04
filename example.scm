(import (prefix (robusta server) robusta/))
(import (prefix (robusta dispatcher) robusta/))
(import (prefix (robusta encoding html) html/))
(import (only (robusta server) ->string))

(import (prefix (robusta encoding json) json/))

(define (index request)
  `((code . 200)
    (headers . ((Content-type . "text/html")))
    (content . ,(html/encode `(html (head) (body (p ,(->string request))))))))

;; (define (about request)
;;   `((code . 200)
;;     (headers . ((Content-type . "text/html")))
;;     (content . ,(->string
;;                   (list "this is an about page<br>"
;;                         "your request: " request)))))

(define dispatcher
  (robusta/dispatcher
   `(("m/\\/(index)?$/" . ,index)
     ("m/static\\/.*/" . ,(λ (req) (robusta/static-dispatcher "static/" req))))))

      ;; ("/api" . ,(lambda (req)
      ;;              `((code . 200)
      ;;                (headers . ((Content-type "application/json")))
      ;;                (content . ,(json/encode '((a . "10") (b .  #f))))))))))

(λ (args)
  (robusta/bind 8080 dispatcher)
  )
