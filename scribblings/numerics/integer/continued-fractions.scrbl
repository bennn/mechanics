#lang scribble/manual

@require[mechanics/private/numerics/integer/continued-fractions]
@require[scribble/eval]

@title[#:tag "continued-fractions"]{Continued Fractions}
@defmodule[mechanics/private/numerics/integer/continued-fractions]
@(define cf-eval
  (make-base-eval
    '(begin (require mechanics/private/numerics/integer/continued-fractions
                     (only-in racket/math pi)))))

The @hyperlink["http://en.wikipedia.org/wiki/Continued_fraction"]{continued fraction} representation of a real number @tt{n} is a stream of integers @tt{n_0, n_1, n_2 ...} such that @tt{(= n (+ n_0 (/ 1 (+ n_1 (/ 1 (+ n_2 ...))))))}.

@defproc[(continued-fraction [n real?]) (sequenceof integer?)]{
  Return the continued fraction representation of a number.
}

@examples[#:eval cf-eval
  (for/list ([n (continued-fraction pi)]
             [i (in-range 5)])
    n)
]

@defproc[(convergents [i* (sequenceof integer?)]) (sequenceof exact-rational?)]{
  Approximate a continued fraction.
  The @tt{n}th term of the result is the number obtained from the first @tt{n} terms of the continued fraction.
}

@examples[#:eval cf-eval
  (for/list ([n (convergents (continued-fraction pi))]
             [i (in-range 3)])
    n)
]
