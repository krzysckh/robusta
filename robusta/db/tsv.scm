#| doc
tsv thingy
|#
(define-library
  (robusta db tsv)

  (import
   (owl toplevel)
   (owl lazy)
   (prefix (owl sys) sys/))

  (export
   create
   open
   insert-into
   close
   get-all
   get-column
   )

  (begin
    (define (open-f fname schema)
      (let* ((exists (sys/file? fname))
             (fo (open-append-file fname))
             (fi (open-input-file fname)))
        (when (not exists)
          (sys/chmod fname #o777 0))
        (list
         `(fi . ,fi)
         `(fo . ,fo)
         `(check-schema
           . ,(λ (lst)
                (and
                 (= (length lst) (length schema))
                 (all
                  (λ (x) x)
                  (map
                   (λ (n)
                     (let ((check-f (cadr (list-ref schema n)))
                           (thing (list-ref lst n)))
                       (check-f thing)))
                   (iota 0 1 (length schema)))))))
         `(serialize
           . ,(λ (lst)
                (let ((s (foldr
                          (λ (acc s) (string-append acc "\t" s))
                          "" (map (λ (n)
                                    ((list-ref (list-ref schema n) 2) (list-ref lst n)))
                                  (iota 0 1 (length lst))))))
                  (substring s 0 (- (string-length s) 1)))))
         `(columns . ,(map car schema))
         )))

    (define (getvs th)
      (let ((chk (cdr (assq 'check-schema th)))
            (fi (cdr (assq 'fi th)))
            (fo (cdr (assq 'fo th)))
            (serialize (cdr (assq 'serialize th)))
            (columns (cdr (assq 'columns th))))
        (values chk fi fo serialize columns)))

    (define (create fname schema)
      (lets ((ret (open-f fname schema))
             (c fi fo s cs (getvs ret))
             (schema-str (foldr (λ (acc s) (string-append acc "\t" s))
                                ""
                                (map (λ (t) (if (string? t) t (symbol->string t)))
                                     (map car schema)))))
        (print-to fo schema-str)
        ret))

    (define (open fname schema)
      (open-f fname schema))

    (define (insert-into th lst)
      (lets ((chk fi fo serialize columns (getvs th)))
       (if (chk lst)
           (print-to fo (serialize lst))
           (error "(robusta db tsv): " "schema-error"))))

    (define (close th)
      (lets ((c fi fo s cs (getvs th)))
        (close-port fi)
        (close-port fo)))

    (define cuttab (string->regex "c/\t/"))
    ;; th → ll
    (define (get-all th)
      (lets ((chk fi fo serialize cs (getvs th)))
        (lmap cuttab (lcdr (lines fi)))))

    (define (get-coln columns colsym acc)
      (cond
       ((null? columns) #f)
       ((eqv? colsym (car columns)) acc)
       (else
        (get-coln (cdr columns) colsym (+ acc 1)))))

    ;; th sym → ll;
    (define (get-column th colsym)
      (lets ((chk fi fo serialize columns (getvs th))
             (n (get-coln columns colsym 0)))
        (if n
            (lmap (λ (v) (print v) (list-ref v n)) (get-all th))
            (error "(robusta db tsv)" "no-such-column"))))
    ))
