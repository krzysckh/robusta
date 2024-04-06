#| doc
urlencode, urldecode
|#
(define-library
  (robusta encoding url)

  (import
   (owl toplevel)
   (owl unicode))

  (export
   decode)

  (begin
    (define (decode-lst l acc)
      (cond
       ((null? l) acc)
       ((eqv? (car l) #\+) (decode-lst (cdr l) (append acc '(#\space))))
       ((eqv? (car l) #\%)
        (decode-lst (cdddr l) (append acc (list (read (string #\# #\x (cadr l) (caddr l)))))))
       (else
        (decode-lst (cdr l) (append acc (list (car l)))))))

    (define (decode s)
      (list->string (utf8-decode (decode-lst (string->list s) '()))))))
