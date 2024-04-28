#| doc
b64 encoding, decoding
|#
(define-library
    (robusta encoding base64)

  (import
   (owl toplevel)
   (owl proof))

  (export
   list->base64
   base64->list
   string->base64
   base64->string
   encode
   decode)

  (begin
    (define alpha "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

    (define (split-every l n pad)
      (let loop ((l l) (acc '()) (npad 0))
        (cond
         ((null? l) (values acc npad))
         (else
          (let* ((r (take l n))
                 (R (if (= (length r) n) r (append r (make-list (- n (length r)) pad)))))
            (loop (drop l n) (append acc (list R)) (- n (length r))))))))

    (define (string-find s c)
      (if (eqv? c #\=)
          #f
          (let loop ((l (string->list s)) (n 0))
            (if (eqv? (car l) c)
                n
                (loop (cdr l) (+ n 1))))))


    (define (list->base64 l)
      (lets ((gs npad (split-every l 3 0))
             (ats (fold append '()
                        (map (λ (l)
                               (let* ((a (car l)) (b (cadr l)) (c (caddr l))
                                      (num (bior (bior (<< a 16) (<< b 8)) c))
                                      (a (band num         #b111111))
                                      (b (band (>> num 6)  #b111111))
                                      (c (band (>> num 12) #b111111))
                                      (d (band (>> num 18) #b111111)))
                                 (list d c b a)))
                             gs))))
            (string-append
             (bytes->string (map (λ (x) (string-ref alpha x)) (take ats (- (length ats) npad))))
             (make-string npad #\=))))

    (define (base64->list s)
      (lets ((l (filter self (map (λ (c) (string-find alpha c)) (string->list s))))
             (gs npad (split-every l 4 0))
             (chrs
              (fold append '()
                    (map (λ (l)
                           (let* ((a (car l)) (b (cadr l)) (c (caddr l)) (d (cadddr l))
                                  (num (bior (<< a 18) (bior (<< b 12) (bior (<< c 6) d))))
                                  (a (band num #xff))
                                  (b (band (>> num 8) #xff))
                                  (c (band (>> num 16) #xff)))
                             (list c b a)))
                         gs))))
            (take chrs (- (length chrs) npad))))


    (define (string->base64 s)
      (list->base64 (string->bytes s)))

    (define (base64->string s)
      (bytes->string (base64->list s)))

    (define encode list->base64)
    (define decode base64->list)

    (example
     (base64->string (string->base64 "abcd")) = "abcd")

    ))
