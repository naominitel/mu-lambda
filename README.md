# The μλ programming language

μλ is a programming language designed to serve as an experiment laboratory for
type system design and implementation.

μλ is built as a [Racket](http://racket-lang.org/) `#lang`-language, similarly
to [typed/racket](). But unlike `typed/racket`, μλ isn't designed with source-
compatibility with Racket as a goal, allowing a completely different language.
Racket is used just as a (very good) extensible front-end and backend.

Installing is done with `raco`:

```
cd mu-lambda
raco pkg install
```

Next versions can be installed by upgrading the package from anywhere is the
source tree has been updated:

```
raco pkg update --link mu-lambda
```

The language can then be used in any Racket source file:

```racket
; test.rkt
#lang mu-lambda
```

```
racket test.rkt
```
