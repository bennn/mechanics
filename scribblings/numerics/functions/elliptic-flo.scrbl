#lang scribble/manual

@(require
  (for-label
   racket
   mechanics/private/numerics/functions/bessjs))

@title{elliptic-flo}

@defmodule[mechanics/private/numerics/functions/elliptic-flo]

This module provides aliases of the complete elliptic integrals for
floating point numbers.

@section{Floating Point}

@defproc[
(first-elliptic-integral
 [k (and/c flonum? positive? (</c 1))])
number?
]

The first complete elliptic integral using floating point arithmetic.

@defproc[
(second-elliptic-integral
 [k (and/c flonum? positive? (</c 1))])
number?
]

The second complete elliptic integral using floating point arithmetic.

@defproc[
(elliptic-integrals
 [k (and/c flonum? positive? (</c 1))])
(cons/c flonum? flonum?)
]

Returns a dotted list whose @racket[cons] is the first complete
elliptic integral at @racket[_k] and whose @racket[cdr] is the second
complete elliptic integral at @racket[_k].