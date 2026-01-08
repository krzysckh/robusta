#| doc
s-expressions → html

this library was originally written for [chai](https://github.com/krzysckh/chai) but development will continue here.
|#
(define-library
    (robusta encoding html)

  (import
   (owl toplevel)
   (only (robusta common) ->string))

  (export
   encode*/printer
   encode/printer
   encode*
   encode
   make-streamer
   )

  (begin
    ;; https://github.com/jquery/jquery-migrate/issues/444
    (define self-closing-tags
      '(area base br col embed hr img input link meta param source track wbr))

    ;; TODO: implement this
    (define (string->safe-html-string s)
      s)

    (define (maybe-close tag self-closing-tags)
      (if (has? self-closing-tags tag)
          ""
          (str "</" tag ">")))

    (define (string->quote-quotes-string s)
      (str-replace s "\"" "&quot;"))

    (define (stringify-kv thing)
      (fold (λ (a b) (str a (car b) "=\"" (string->quote-quotes-string (cdr b)) "\" ")) "" thing))

    (define (encode*/printer lst self-closing-tags f)
      (let ((f (let walk ((it lst) (f f))
                 (cond
                  ((string? it)
                   (f it))
                  ((number? it)
                   (f (str it)))
                  ((symbol? it)
                   (f (str it)))
                  ((list? (car it))
                   (lets ((f (f (str "<" (caar it) " " (stringify-kv (cdar it)) ">")))
                          (f (fold (λ (f v) (walk v f)) f (cdr it)))
                          (f (f (maybe-close (caar it) self-closing-tags))))
                     f))
                  (else
                   (lets ((f (f (str "<" (car it) ">")))
                          (f (fold (λ (f v) (walk v f)) f (cdr it)))
                          (f (f (maybe-close (car it) self-closing-tags))))
                     f))))))
        (f (eof-object))))

    (define (encode* lst self-closing-tags)
      (let walk ((it lst))
        (cond
         ((string? it) it)
         ((number? it) (str it))
         ((symbol? it) (str it))
         ((list? (car it))
          (str
           "<" (caar it) " " (stringify-kv (cdar it)) ">"
           (fold str "" (map walk (cdr it)))
           (maybe-close (caar it) self-closing-tags)
           ))
         (else
          (str
           "<" (car it) ">"
           (fold string-append "" (map walk (cdr it)))
           (maybe-close (car it) self-closing-tags))
          ))))

    (define encode         (C encode* self-closing-tags))
    (define (encode/printer lst f) (encode*/printer lst self-closing-tags f))

    (define (string-lengths lst)
      (fold (λ (a b) (+ a (string-length b))) 0 lst))

    ;; buffer-size is NOT exact - it WILL overflow
    (define (make-streamer f inexact-buffer-size)
      (letrec ((streamer
                (λ (acc)
                  (λ (s*)
                    (lets ((acc* (cons s* acc)))
                      (cond
                       ((eof-object? s*) ; end of stream
                        (f (string-concatenate (reverse acc)))
                        (f s*))
                       ((> (string-lengths acc*) inexact-buffer-size)
                        (f (string-concatenate (reverse acc*)))
                        (streamer #n))
                       (else
                        (streamer acc*))))))))
        (streamer #n)))
    ))
