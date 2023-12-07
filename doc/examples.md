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
