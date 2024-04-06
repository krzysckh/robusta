## web framework for owl lisp.

<img src="https://raw.githubusercontent.com/krzysckh/robusta/master/doc/robusta-man.jpg" align="right" width="300px">

Robusta is a set of almost-ready-to-use™ tools that can make writing web stuff <strike>even less</strike> enjoyable.

Robusta consists of:
* `(robusta server)` - simple async tcp listener,
* `(robusta dispatcher)` - regex-based dispatchers for `(robusta server)`,
* `(robusta http)` - parsing HTTP,
* `(robusta mime)` - builtin MIME table,
* `(robusta encoding json)` - json encoder and decoder *(very slow)*,
* `(robusta encoding html)` - html encoder,
* `(robusta encoding url)` - decoder for urlencoded strings

## how does it look like?

see [doc/examples.md](doc/examples.md)

## more docs

* `make doc` or [this](https://pub.krzysckh.org/robusta.html)

