#lang scribble/manual

@require[mechanics/private/numerics/integer/farey]
@require[scribble/eval]

@title[#:tag "farey"]{Farey Sequences}
@defmodule[mechanics/private/numerics/integer/farey]
@(define farey-eval
  (make-base-eval
    '(begin (require mechanics/private/numerics/integer/farey))))

@hyperlink["http://mathworld.wolfram.com/FareySequence.html"]{Farey Sequences} enumerate reduced fractions.

@defproc[(farey [lo integer?] [hi integer?]) (-> integer? (listof integer?))]{
  Given a lower and upper bound, return a function on naturals @racket[n] that enumerates all fractions between @racket[lo] and @racket[hi] inclusive, with denominators no greater than @racket[n].
}

@examples[#:eval farey-eval
  ((farey 0 1) 1)
  ((farey 0 1) 5)
]
