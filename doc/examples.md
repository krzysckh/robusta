# examples

### simple *hello* server without dispatchers
```scheme
(import (robusta server))

(bind
  8080
  (lambda (r)
    (respond
     r (response
        code    => 200
        content => "hello!"
        headers => '((Content-type . "text/html"))))))
```

### simple server *with* dispatchers

```scheme
(import
 (robusta server)
 (robusta dispatcher))

(define (index request)
  (response
   code    => 200
   headers => '((Content-type . "text/html"))
   content => "this is the index page"))

(define (about request)
  (response
   code => 200
   headers => '((Content-type . "text/html"))
   content => "this is an about page"))

(define dispatcher (make-dispatcher
                    "/"      => index
                    "/about" => about))

(bind 8080 dispatcher)
```

### streamed response from a route

```scheme
(import
 (robusta full))

(define app
  (make-dispatcher
   "/" => (λ (req)
            (response
             content => (λ (stream)
                          (let loop ((n 10))
                            (if (= n 0)
                                #t
                                (begin
                                  (stream '(#\a 10))
                                  (sleep 500)
                                  (loop (- n 1))))))))))

(bind 8080 app)
```

### http basic auth

```scheme
(import
 (robusta server)
 (robusta dispatcher))

(define app
  (make-dispatcher
   "/" => (λ (req)
            (with-http-basic-auth (req "login" "password")
              (response
               code    => 200
               headers => '((Content-type . "text/html"))
               content => "ok")))))

(bind 8080 app)
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
(import
 (prefix (robusta server) robusta/)
 (prefix (robusta dispatcher) robusta/)
 (prefix (robusta encoding html) html/))

(define (idx request)
  (robusta/response
   code    => 200
   headers => '((Content-type . "text/html"))
   content => (html/encode '(html (head) (body (p "this the only page"))))))

(robusta/bind 8080 (robusta/make-dispatcher "m/^.*$/" => idx))
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

(define (static! req) (static-dispatcher "static" "/static/" req))
;;                                        ^       ^
;;                                        |       \_ static route url base
;;                                        \_ directory with static files


(define dis (make-dispatcher
             "m/static(\\/.*)?/" => static!
             "m/\\/?/"           => (λ (req)
                                      (response
                                       code    => 200
                                       headers => '((Content-type . "text/html"))
                                       content => index-html))))

(bind 8080 dis)
```

### simple app with a logger

```scheme
(import
 (robusta server)
 (robusta log)
 (robusta dispatcher))

(define logger (make-stdout-logger))
(define app (make-dispatcher "/" => (λ _ (response content => "hello"))))

(bind 8080 app logger)
```

### POST example

```scheme
(import
 (robusta server)
 (prefix (robusta dispatcher) D/)
 (prefix (robusta encoding html) html/))

(define dispatcher
  (D/make-dispatcher
   "/" => (λ (req)
            (let* ((M (get req 'method #f))
                   (n (if (eqv? M 'GET)
                          0
                          (string->number (cdr (assq 'n (get req 'post-data #n)))))))
              (response
               code    => 200
               headers => '((Content-type . "text/html"))
               content => (html/encode
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
                              (button "-")))))))))

(bind 8080 dispatcher)
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
