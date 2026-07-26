#| doc
s-expressions → rss

example:

```scheme
(rss/encode "rss data" "http://localhost" "this is the description"
            (list
              (ff 'pubDate (time)
                  'title "Hello, World!"
                  'description "this is the desc")
              (ff 'pubDate (+ (time) 10000)
                  'title "Hello, World 2!"
                  'description "this is the desc in the future")))
```
creates an rss feed with two items
|#

(define-library (robusta encoding rss)
  (import
   (owl toplevel)
   (only (robusta encoding html) encode*))

  (export
   encode)

  (begin
    (define month-names (tuple "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

    (define (date-rfc822 yr mon day)
      (lets ((_ d (week-info day mon yr)))
        (format
         #f "~a, ~2,'0d ~a ~d 00:00:01 +0200"
         (substring (ref day-names-en d) 0 3)
         day (ref month-names mon) yr)))

    (define (date-rfc822-hr yr mon day hr min sec)
      (lets ((_ d (week-info day mon yr)))
        (format
         #f "~a, ~2,'0d ~a ~d ~2,'0d:~2,'0d:~2,'0d +0200"
         (substring (ref day-names-en d) 0 3)
         day (ref month-names mon) yr
         hr min sec)))

    ;; some-timestamp → rfc822 timestamp
    (define (endate d)
      (cond
       ((number? d) (lets ((day mon yr hr min sec (date d)))
                      (date-rfc822-hr yr mon day hr min sec)))
       ((list? d)
        (case (len d)
          (3 (date-rfc822 (car d) (cadr d) (caddr d)))
          (6 (date-rfc822-hr (car d) (cadr d) (caddr d) (cadddr d) (lref d 4) (lref d 5)))
          (else
           (error "invalid timestamp " d))))
       ((string? d) d)
       (else
        (error "invalid date format: " d))))

    ;; items is a list of (ff of key -> val) where key is one of rss keys and value is its value
    ;; holy shit this sentence is so unhelpful
    (define (encode title site-url description items)
      (string-append
       "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"
       (encode*
        `((rss (version . "2.0"))
          (channel
           (title ,title)
           (link ,site-url)
           (description ,description)
           ,@(map (λ (ob*)
                    (let ((ob (if-lets ((d (or (get ob* 'pubDate)
                                               (get ob* 'date)
                                               (get ob* 'publish-date))))
                                (put ob* 'pubDate (endate d))
                                (put ob* 'pubDate (endate (time))))))
                      `(item ,@(ff-fold (λ (a k v) (cons (list k v) a)) #n ob))))
                  items)))
        #n)))
    ))
