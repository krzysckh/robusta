#| doc
this rocks actually
|#

(define-library (robusta experimental db)
  (import
   (owl toplevel)
   (owl unicode)
   (prefix (ext sqlite io) s3/))

  (export
   execute*
   start-migrate-thread
   add-migration
   start-schema-thread
   add-column
   get-migrator
   get-schema
   table-has-column?
   list->sql-list
   define-table
   db-refresher
   db
   db-get
   db-get*
   db-get-where
   )

  (begin

    (define (execute* ptr s arg)
      (map
       (λ (l)
         (map
          (λ (v)
            (if (string? v)
                (bytes->string (utf8-decode (string->bytes v)))
                v))
          l))
       (s3/execute ptr s arg)))

    (define (start-migrate-thread)
      (thread
       'migrate
       (let loop ((fs #n))
         (lets ((who v (next-mail)))
           (tuple-case v
             ((add f) (loop (cons f fs)))
             ((dump) (mail who fs))
             ;; ((migrate ptr)
             ;;  (for-each (λ (tbl) (tbl ptr)) fs)
             ;;  (mail who 'ok)
             ;;  (loop #n))
             (else
              (loop fs)))))))

    (define (add-migration f)
      (mail 'migrate (tuple 'add f)))

    (define (add-column table column t)
      (mail 'schema (tuple 'add-column table column t)))

    ;; TODO: hack ish
    (define (get-migrator)
      (let ((fs (interact 'migrate (tuple 'dump))))
        (λ (p)
          (for-each (λ (f) (f p)) fs))))

    (define (get-schema table)
      (interact 'schema (tuple 'get-schema table)))

    (define (start-schema-thread schema)
      (thread
       'schema
       (let loop ((schema schema)) ; ff of table -> k -> v
         (lets ((who v (next-mail)))
           (tuple-case v
             ((add-column table column t)
              (loop (put schema table (put (get schema table) column t))))
             ((get-schema table)
              (mail who (get schema table))
              (loop schema))
             ((dump)
              (mail who schema))))))) ; die

    ;; unsafe
    (define (table-has-column? ptr name col)
      (let ((v (execute* ptr (str "SELECT * FROM pragma_table_info('" name "') WHERE name = ?") (list (str col)))))
        (> (len v) 0)))

    (define (list->sql-list lst)
      (cond
       ((null? lst) "")
       ((> (len lst) 1)
        (fold (λ (a b) (str a ", " b)) (car lst) (cdr lst)))
       (else ; = 1
        (str (car lst)))))

    ;; unsafe
    (define-syntax define-table
      (syntax-rules (42 => relation _getk _gets keys migrate id int)
        ((_ 42 name)
         (λ _ (print (str "[define-table] ok: " 'name))))
        ;; ((_ 42 name k => (v ...) . rest)
        ;;  (λ (ptr)
        ;;    (when (not (table-has-column? ptr 'name 'k))
        ;;      (print "[define-table] new column for migration: " 'k)
        ;;      (s3/execute ptr (str "ALTER TABLE " 'name " ADD COLUMN " 'k " " (fold (λ (a b) (str a " " b)) "" '(v ...)) ";")))
        ;;    ((_ 42 name . rest) ptr)))
        ((_ 42 name k => (relation v) . rest)
         (λ (ptr)
           (when (not (table-has-column? ptr 'name 'k))
             (print "[define-table] new column for migration: " 'k)
             (s3/execute ptr (str "ALTER TABLE " 'name " ADD COLUMN " 'k " int;")))
           ((_ 42 name . rest) ptr)))
        ((_ 42 name k => v . rest)
         (λ (ptr)
           (when (not (table-has-column? ptr 'name 'k))
             (print "[define-table] new column for migration: " 'k)
             (s3/execute ptr (str "ALTER TABLE " 'name " ADD COLUMN " 'k " " 'v ";")))
           ((_ 42 name . rest) ptr)))
        ((_ _getk k => v . rest)
         (cons 'k (_ _getk . rest)))
        ((_ _getk) #n)
        ((_ _gets name k => v . rest)
         (begin
           (add-column 'name 'k 'v)
           (_ _gets name . rest)))
        ((_ _gets name) (add-column 'name 'id 'int))
        ((_ (name constructor updater) . rest)
         (define-values (delivered constructor updater)
           (values
            (begin
              (_ _gets name . rest)
              (add-migration
               (λ (ptr)
                 (s3/execute ptr (str "CREATE TABLE IF NOT EXISTS " 'name " (id integer not null primary key)"))
                 ((_ 42 name . rest) ptr))))
            (λ (ff*)
              (lets ((keys (filter (λ (x) (not (eq? 'nope (get ff* x 'nope)))) (_ _getk . rest))))
                (λ (ptr)
                  (s3/execute
                   ptr
                   (str "INSERT INTO " 'name "(" (fold (λ (a b) (str a ", " b)) (car keys) (cdr keys)) ") VALUES ("
                        (fold (λ (a b) (string-append a ", ?")) "?" (cdr keys)) ")")
                   (map
                    (λ (x) (get ff* x 0))
                    keys)))))
            (λ (ff*)
              (lets ((keys (filter (λ (x) (not (eq? 'nope (get ff* x 'nope)))) (_ _getk . rest))))
                (when (not (get ff* 'id #f))
                  (error (ff->list ff*) " does not contain an id"))
                (λ (ptr)
                  (s3/execute
                   ptr
                   (str "UPDATE " 'name " SET " (fold (λ (a b) (str a ", " b " = ?")) (str (car keys) " = ?") (cdr keys)) " WHERE id = ?")
                   (append (map (λ (x) (get ff* x 0)) keys) (list (get ff* 'id -1)))))))
            )))
        ))

    ;; TODO: changeable 'db sym so multiple databases can be open at the same time
    (define (db-refresher db-file)
      (thread
       'db
       (let loop ((ptr (s3/open db-file)))
         (lets ((who v (next-mail)))
           (tuple-case v
             ((get)
              (mail who ptr)
              (loop ptr))
             ((refresh)
              ;; (print "refreshing database pointer")
              (s3/close ptr)
              (loop (s3/open db-file)))))))
      (thread
       (let loop ()
         (sleep (* 30 1000))
         (mail 'db (tuple 'refresh))
         (loop)))
      )

    (define (db)
      (interact 'db (tuple 'get)))

    (define (db-get table items . id)
      (lets ((p (db)))
        (if (null? id)
            (execute* p (str "SELECT " (list->sql-list items) " FROM " table) #n)
            (car* (execute* p (str "SELECT " (list->sql-list items) " FROM " table " WHERE id = ?") (list (car id)))))))

    ;; db-get bug yields ff
    (define (db-get* table items . id)
      (if (null? id)
          (map
           (λ (l) (list->ff (zip cons items l)))
           (db-get table items))
          (list->ff (zip cons items (db-get table items (car id))))))

    (define (db-get-where table items where arg)
      (lets ((p (db)))
        ;; (print "will execute " (str "SELECT " (list->sql-list items) " FROM " table " " where) arg)
        (execute* p (str "SELECT " (list->sql-list items) " FROM " table " " where) arg)))

    ))
