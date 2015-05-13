#lang scribble/manual

@(require
  (for-label
   racket
   mechanics/private/numerics/functions/bessjs))

@title{bessjs}

@defmodule[mechanics/private/numerics/functions/bessjs]

@defproc[
(bessjs
 [max-order exact-nonnegative-integer?]
 [z number?])
(non-empty-listof number?)
]

Returns a list containing the Bessel functions of the first kind of
order 0 through @racket[_max-order] evaluated at @racket[_z]. The
values are accurate to machine precision when @racket[(< _x 2)] and
large @racket[_max-order].