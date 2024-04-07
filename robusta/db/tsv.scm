#| doc
inneficient tsv dbms.

schema for `open` is:
```scheme
(list
  (col-name type-pred-function thing->string-function string->thing-function)
  ...)
```

for example:

```scheme
(define (self s) s)

`((uname      ,string? ,self           ,self)
  (passwd     ,string? ,self           ,self)
  (secret-num ,number? ,number->string ,string->number))
```

this library uses the opened db in-memory (via `(owl variable)`),
and saves it only on `(save)` and `(close)` calls.

|#
(define-library
  (robusta db tsv)

  (import
   (owl toplevel)
   (owl lazy)
   (owl variable)
   (prefix (owl sys) sys/))

  (export
   open
   insert-into
   close
   get-all
   get-column
   filter-by
   delete-from
   save
   )

  (begin
    (define cuttab (string->regex "c/\t/"))

    (define (getvs th)
      (let ((chk (cdr (assq 'check-schema th)))
            (F (cdr (assq 'F th)))
            (v (cdr (assq 'v th)))
            (serialize (cdr (assq 'serialize th)))
            (deserialize (cdr (assq 'deserialize th)))
            (columns (cdr (assq 'columns th))))
        (values chk v F serialize deserialize columns)))

    ;; TODO: find a way to replace \n with \\n etc.
    (define delnl (string->regex "s/\n//g"))
    (define deltab (string->regex "s/\t//g"))
    (define delr (string->regex "s/\r//g"))

    (define (tsv-safe s)
      (delnl (deltab (delr s))))

    (define (open fname schema)
      (let* ((exists (sys/file? fname))
             (var (make-variable (string->symbol (string-append "dbtsv-" fname)))))
        (when (not exists)
          (sys/chmod fname #o777 0))

        (lets ((ret
               (list
                `(F . ,fname)
                `(v . ,var)
                ;; `(fi . ,fi)
                ;; `(fa . ,fa)
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
                                 ""
                                 (map (λ (n) (tsv-safe ((list-ref (list-ref schema n) 2) (list-ref lst n))))
                                      (iota 0 1 (length lst))))))
                         (substring s 0 (- (string-length s) 1)))))
                `(deserialize
                  . ,(λ (lst)
                       (map
                        (λ (n) ((list-ref (list-ref schema n) 3) (list-ref lst n)))
                        (iota 0 1 (length lst)))))
                `(columns . ,(map car schema))))
               (chk v F serialize ds columns (getvs ret)))

            (if exists
                (let ((fi (open-input-file fname)))
                  (var (map (λ (s) (ds (cuttab s))) (force-ll (lcdr (lines fi))))) ;; WOW
                  (close-port fi))
                (list))
            ret
            )))

    (define (create-schema-str cols)
      (foldr (λ (acc s) (string-append acc "\t" s))
             ""
             (map (λ (t) (if (string? t) t (symbol->string t)))
                  cols)))

    (define (insert-into th lst)
      (lets ((chk v F serialize ds columns (getvs th)))
       (when (chk lst)
         (if (v)
             (v (append (v) (list lst)))
             (v (list lst))))))

    (define (save th)
      (lets ((c v F s ds cs (getvs th))
             (of (open-output-file F)))
        (print-to of (create-schema-str cs))
        (for-each (λ (l) (print-to of (s l))) (v))
        (close-port of)))

    (define (close th)
      (lets ((c v F s ds cs (getvs th)))
        (save th)))

    (define (get-all th)
      (lets ((chk v F serialize ds cs (getvs th)))
        (v)))

    (define (get-coln columns colsym acc)
      (cond
       ((null? columns) #f)
       ((eqv? colsym (car columns)) acc)
       (else
        (get-coln (cdr columns) colsym (+ acc 1)))))

    (define (get-column th colsym)
      (lets ((chk v F serialize ds columns (getvs th))
             (n (get-coln columns colsym 0)))
        (if n
            (map (λ (v) (list-ref v n)) (get-all th))
            (error "(robusta db tsv)" "no-such-column"))))

    ;; TODO: apply ma implementation-restriction dla dużych list iirc
    (define (filter-by th pred)
      (lets ((chk v F serialize ds columns (getvs th)))
        (filter (λ (l) (apply pred l)) (get-all th))))

    (define (delete-from th pred)
      (lets ((chk v F serialize ds columns (getvs th)))
        (let ((kept (filter (λ (l) (not (apply pred l))) (get-all th))))
          (v kept))))
    ))
