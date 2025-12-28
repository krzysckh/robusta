#| doc
i11n

i11nizer should be started before calls to make-i11n

as threads don't survive between compile & runtime (when compiling to binaries),
i11nizer can dump its map as a thunk to later recover data at runtime

for example:

```scheme
;; -*- mode: owl; compile-command: "ol -x c -o - temp.scm | tcc -run -" -*-
(import (robusta i11n))

(start-i11nizer)

(make-i11n (EN)
  main.greet   => "Hello, World!"
  main.farwell => "Goodbye, World!")

(make-i11n (RU)
  main.greet   => "Привет, мир!"
  main.farwell => "Прощай, мир!")

(print-to stderr "  compile-time")

(print-to stderr (i11n-get 'EN 'main.greet))
(print-to stderr (i11n-get 'RU 'main.greet))

(define restart! (interact 'i11n (tuple 'dump)))

(λ (_)
  (print-to stderr "  run-time")
  (restart!)
  (print-to stderr (i11n-get 'EN 'main.farwell))
  (print-to stderr (i11n-get 'RU 'main.farwell))
  0)
```

|#

(define-library
    (robusta i11n)

  (import
   (owl toplevel)
   (owl thread))

  (export
   start-i11nizer
   start-i11nizer*
   make-i11n
   make-i11n-getter
   i11n-get
   )

  (begin
    (define (i11n-get lang it)
      (interact 'i11n (tuple 'get lang it)))

    (define (start-i11nizer* base default)
      (let ((default (if (null? default) "???" default)))
        (thread
         'i11n
         (let loop ((i11n base))
           (lets ((who v (next-mail)))
             (tuple-case v
               ((add lang map)
                (loop (put i11n lang map)))
               ((dump)                      ; dump & die
                (mail who (λ () (start-i11nizer* i11n default))))
               ((get lang it)
                (mail who (get (get i11n lang empty) it default))
                (loop i11n))
               (else
                (print "unknown i11n query: " v)
                (loop i11n))))))))

    (define (start-i11nizer)
      (start-i11nizer* empty #n))

    (define-syntax make-i11n
      (syntax-rules (42 i11n add)
        ((_ 42 f k => v . r)
         (_ 42 (put f 'k v) . r))
        ((_ 42 f) f)
        ((_ (lang) . r)
         (mail 'i11n (tuple 'add 'lang (_ 42 empty . r))))))

    (define (make-i11n-getter lang)
      (λ (it)
        (interact 'i11n (tuple 'get lang it))))

    ))
