#| doc
inneficient tsv dbms.

schema for ``

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
   filter-by
   delete-from
   )

  (begin
    (define (open-f fname schema)
      (let* ((exists (sys/file? fname))
             (fa (open-append-file fname))
             (fi (open-input-file fname)))
        (when (not exists)
          (sys/chmod fname #o777 0))
        (list
         `(F . ,fname)
         `(fi . ,fi)
         `(fa . ,fa)
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
                       (if (check-f thing)
                           #t
                           (error "(robusta db tsv) schema-error: " thing))))
                   (iota 0 1 (length schema)))))))
         `(serialize
           . ,(λ (lst)
                (let ((s (foldr
                          (λ (acc s) (string-append acc "\t" s))
                          "" (map (λ (n)
                                    ((list-ref (list-ref schema n) 2) (list-ref lst n)))
                                  (iota 0 1 (length lst))))))
                  (substring s 0 (- (string-length s) 1)))))
         `(deserialize
           . ,(λ (lst)
                (map
                 (λ (n) ((list-ref (list-ref schema n) 3) (list-ref lst n)))
                 (iota 0 1 (length lst)))))

         `(columns . ,(map car schema))
         )))

    (define (getvs th)
      (let ((chk (cdr (assq 'check-schema th)))
            (fi (cdr (assq 'fi th)))
            (fa (cdr (assq 'fa th)))
            (F (cdr (assq 'F th)))
            (serialize (cdr (assq 'serialize th)))
            (deserialize (cdr (assq 'deserialize th)))
            (columns (cdr (assq 'columns th))))
        (values chk fi fa F serialize deserialize columns)))

    (define (create-schema-str cols)
      (foldr (λ (acc s) (string-append acc "\t" s))
             ""
             (map (λ (t) (if (string? t) t (symbol->string t)))
                  cols)))

    (define (create fname schema)
      (when (sys/file? fname)
        (error "(robusta db tsv) file already exists, don't (create) - (open) " fname))
      (let ((ret (open-f fname schema)))
        (lets ((c fi fa F s ds cs (getvs ret))
               (schema-str (create-schema-str cs)))
          (print-to fa schema-str)
          ret)))

    (define (open fname schema)
      (open-f fname schema))

    (define (insert-into th lst)
      (lets ((chk fi fa F serialize ds columns (getvs th)))
       (when (chk lst)
         (print-to fa (serialize lst)))))

    (define (close th)
      (lets ((c fi fa F s ds cs (getvs th)))
        (close-port fi)
        (close-port fa)))

    (define cuttab (string->regex "c/\t/"))
    ;; th → ll
    (define (get-all th)
      (lets ((chk fi fa F serialize ds cs (getvs th)))
        ;; (sys/seek-set fi 0)
        ;; (print (force-ll (lines fi)))
        (sys/seek-set fi 0)
        ;; (lmap (λ (s) (ds (cuttab s))) (lcdr (lines fi)))))
        (lmap (λ (s) (ds (cuttab s))) (lcdr (byte-stream->lines (file->byte-stream fi))))))

    (define (get-coln columns colsym acc)
      (cond
       ((null? columns) #f)
       ((eqv? colsym (car columns)) acc)
       (else
        (get-coln (cdr columns) colsym (+ acc 1)))))

    ;; th sym → ll;
    (define (get-column th colsym)
      (lets ((chk fi fa F serialize ds columns (getvs th))
             (n (get-coln columns colsym 0)))
        (if n
            (lmap (λ (v) (list-ref v n)) (get-all th))
            (error "(robusta db tsv)" "no-such-column"))))

    ;; TODO: apply ma implementation-restriction dla dużych list iirc
    (define (filter-by th pred)
      (lets ((chk fi fa F serialize ds columns (getvs th)))
        (lkeep (λ (l) (apply pred l)) (get-all th))))

    (define (delete-from th pred)
      (lets ((chk fi fa F serialize ds columns (getvs th)))
        (let ((kept (force-ll (lremove (λ (l) (apply pred l)) (get-all th)))))
          ;; (sys/close
           (sys/open F (sys/O_TRUNC) #o777)
           ;; )
          (print-to fa (create-schema-str columns))
          (map (λ (v) (print v) (insert-into th v)) kept))))
    ))
