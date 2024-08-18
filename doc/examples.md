# examples

### simple *hello* server without dispatchers
```scheme
(import (prefix (robusta server) robusta/))

(robusta/bind
  8080
  (lambda (r)
    (print r)
    (let ((send (cdr (assq 'send r))))
      (send 200 '((Content-type . "text/html")) "hello!"))))
```

### simple server *with* dispatchers

```scheme
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
```

### using (robusta encoding json)

```scheme
(import (prefix (robusta encoding json) json/))

(define json-string "[1, 2, [3], [[4]], [[[5]]], [[[[6]]]], 7]")
(define json-structure '((a . "10") (b . #f)))
(print (json/decode json-string)) ; → (1 2 (3) ((4)) (((5))) ((((6)))) 7)
(print (json/encode json-structure)) ; → {"a":"10","b":false}
```

### using (robusta encoding html)

```scheme
(import (prefix (robusta server) robusta/))
(import (prefix (robusta dispatcher) robusta/))
(import (prefix (robusta encoding html) html/))

(define (idx request)
  `((code . 200)
    (headers . ((Content-type . "text/html")))
    (content . ,(html/encode '(html (head) (body (p "this the only page")))))))

(robusta/bind 8080 (robusta/dispatcher `(("m/^.*$/" . ,idx))))
```

### static pages with an index dispatcher

```scheme
(import
 (robusta server)
 (robusta dispatcher)
 (prefix (robusta encoding html) html/))

(define index-html
  (html/encode
   `(html
     (head)
     (body "see the static pages" ((a (href . "static/")) "here")))))

(define dis (dispatcher
             `(("m/static(\\/.*)?/" . ,(λ (req) (static-dispatcher "static/" req)))
               ("m/\\/?/" . ,(λ (req) `((code . 200)
                                        (headers . ((Content-type . "text/html")))
                                        (content . ,index-html)))))))

(λ (_) (bind 8000 dis))
```

### POST example

```scheme
(import
 (robusta server)
 (prefix (robusta dispatcher) D/)
 (prefix (robusta encoding html) html/))

(define dispatcher
  (D/dispatcher
   `(("/" . ,(λ (req)
               (let* ((M (cdr (assq 'method req)))
                      (n (if (eqv? M 'GET)
                             0
                             (string->number (cdr (assq 'n (cdr (assq 'post-data req))))))))
                 `((code . 200)
                   (headers . ((Content-type . "text/html")))
                   (content
                    . ,(html/encode
                        `(body
                          (p "n: " ,n)
                          ((form (method . "POST"))
                           ((input (type . "hidden")
                                   (name . "n")
                                   (value . ,(number->string (+ n 1)))))
                           (button "+"))
                          ((form (method . "POST"))
                           ((input (type . "hidden")
                                   (name . "n")
                                   (value . ,(number->string (- n 1)))))
                           (button "-"))))))))))))

(λ (_) (bind 6969 dispatcher))
```

### tsv database

```scheme
(import
 (prefix (robusta db tsv) tsv/)
 (prefix (owl sys) sys/))

(define (self s) s)
(define dbname "db.tsv")

(define schema
;;   col-name  type-pred  thing->string  string->thing
  `((uname      ,string? ,self           ,self)
    (passwd     ,string? ,self           ,self)
    (secret-num ,number? ,number->string ,string->number)))

(when (sys/file? dbname)
  (sys/unlink dbname))

;; th = tsv handle
(define th (tsv/open "db.tsv" schema))

(tsv/insert-into th '("admin" "zaq1@WSX" 10))
(tsv/insert-into th '("user0" "helloworld" 120))

(print (tsv/filter-by th (λ (u p s) (= s 120))))
(print (tsv/get-column th 'passwd))
(tsv/delete-from th (λ (u p s) (string-ci=? u "USER0")))
(print (tsv/get-all th))

(tsv/close th)
```

### url encoding/decoding

```scheme
(import (prefix (robusta encoding url) url/))

(define al '((a . b) (c . d) (e . 10) (f . "1") (g . #false)))

;; everything str'd as types get stripped
(print (string=? (str (url/decode-form (url/encode al))) (str al)))
```
