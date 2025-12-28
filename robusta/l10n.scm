#| doc
l10n

l10nizer should be started before calls to make-l10n

as threads don't survive between compile & runtime (when compiling to binaries),
l10nizer can dump its map as a thunk to later recover data at runtime

for example:

```scheme
;; -*- mode: owl; compile-command: "ol -x c -o - temp.scm | tcc -run -" -*-
(import (robusta l10n))

(start-l10nizer)

(make-l10n (EN)
  main.greet   => "Hello, World!"
  main.farwell => "Goodbye, World!")

(make-l10n (RU)
  main.greet   => "Привет, мир!"
  main.farwell => "Прощай, мир!")

(print-to stderr "  compile-time")

(print-to stderr (l10n-get 'EN 'main.greet))
(print-to stderr (l10n-get 'RU 'main.greet))

(define restart! (interact 'l10n (tuple 'dump)))

(λ (_)
  (print-to stderr "  run-time")
  (restart!)
  (print-to stderr (l10n-get 'EN 'main.farwell))
  (print-to stderr (l10n-get 'RU 'main.farwell))
  0)
```

|#

(define-library
    (robusta l10n)

  (import
   (owl toplevel)
   (owl thread))

  (export
   start-l10nizer
   start-l10nizer*
   make-l10n
   make-l10n-getter
   l10n-get
   )

  (begin
    (define (l10n-get lang it)
      (interact 'l10n (tuple 'get lang it)))

    (define (start-l10nizer* base default)
      (let ((default (if (null? default) "???" default)))
        (thread
         'l10n
         (let loop ((l10n base))
           (lets ((who v (next-mail)))
             (tuple-case v
               ((add lang map)
                (loop (put l10n lang map)))
               ((dump)                      ; dump & die
                (mail who (λ () (start-l10nizer* l10n default))))
               ((get lang it)
                (mail who (get (get l10n lang empty) it default))
                (loop l10n))
               (else
                (print "unknown l10n query: " v)
                (loop l10n))))))))

    (define (start-l10nizer)
      (start-l10nizer* empty #n))

    (define-syntax make-l10n
      (syntax-rules (42 l10n add)
        ((_ 42 f k => v . r)
         (_ 42 (put f 'k v) . r))
        ((_ 42 f) f)
        ((_ (lang) . r)
         (mail 'l10n (tuple 'add 'lang (_ 42 empty . r))))))

    (define (make-l10n-getter lang)
      (λ (it)
        (interact 'l10n (tuple 'get lang it))))

    ))
