#| doc
middleware defining loggers compatible with api used by `bind` and `fastcgi-bind`
|#
(define-library
    (robusta log)

  (import
   (owl toplevel)
   )

  (export
   common-date
   make-logger
   make-file-logger
   make-stdout-logger
   )

  (begin
    (define month-names
      '(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec))

    ;; TODO: +0000
    (define (common-date)
      (lets ((d M y h m s (date)))
        (format #f "[~2,'0d/~a/~d:~2,'0d:~2,'0d:~2,'0d +0000]" d (lref month-names (- M 1)) y h m s)))

    (define (make-logger fn)
      (λ (req code siz)
        (let ((ip (get req 'ip (bytevector 127 0 0 1))))
          (fn (format #f "~d.~d.~d.~d - - ~a \"~a ~a ~a\" ~a ~a"
                      (bytevector-u8-ref ip 0)
                      (bytevector-u8-ref ip 1)
                      (bytevector-u8-ref ip 2)
                      (bytevector-u8-ref ip 3)
                      (common-date)
                      (get req 'method "UNKNOWN")
                      (get req 'path   "UNKNOWN/PATH")
                      (get req 'protocol "HTTP/1.1")
                      code siz)))))

    (define (make-file-logger filename)
      (make-logger (λ (str)
                     (let ((f (open-append-file filename)))
                       (write-bytes f (string->bytes str))
                       (write-bytes f '(10))
                       (close-port f)))))

    (define (make-stdout-logger)
      (make-logger (H print-to stdout)))

    ))
