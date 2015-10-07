#lang scribble/manual

@(require
  (for-label
    racket
    mechanics/private/numerics/functions/elliptic))

@title{elliptic}

@defmodule[mechanics/private/numerics/functions/elliptic]

@section{Carlon Symmetric Forms}

Defines the canonical set of elliptic integrals: the
@link["http://en.wikipedia.org/wiki/Carlson_symmetric_form"]{Carlson
symmetric forms}. Many aliases are provided for backward compatibility
with @racket[scmutils].

@defproc[
(Rf [x (not/c (and/c real? negative?))]
    [y (not/c (and/c real? negative?))]
    [z (not/c (and/c real? negative?))])
number?
]

Defines the first Carlson symmetric form, @italic{R@subscript{F}}. The
restriction on the inputs is to avoid numerical instability branch cut
on the negative real axis.

@defproc[
(Carlson-elliptic₁ [x (not/c (and/c real? negative?))]
                   [y (not/c (and/c real? negative?))]
                   [z (not/c (and/c real? negative?))])
number?
]

Alias for @racket[Rf].

@defproc[
(Carlson-elliptic-1 [x (not/c (and/c real? negative?))]
                    [y (not/c (and/c real? negative?))]
                    [z (not/c (and/c real? negative?))])
number?
]

Alias for @racket[Rf].

@defproc[
(Carlson-elliptic₁-simple [x (not/c (and/c real? negative?))]
                          [y (not/c (and/c real? negative?))]
                          [z (not/c (and/c real? negative?))])
number?
]

Same as @racket[Rf], but with less numerical precision.

@defproc[
(Carlson-elliptic-1-simple [x (not/c (and/c real? negative?))]
                           [y (not/c (and/c real? negative?))]
                           [z (not/c (and/c real? negative?))])
number?
]

Alias for @racket[Carlson-elliptic₁-simple].

@defproc[
(Rd [x (not/c (and/c real? negative?))]
    [y (not/c (and/c real? negative?))]
    [z (not/c (and/c real? negative?))])
number?
]

Defines the second Carlson symmetric form @italic{R@subscript{D}}.

@defproc[
(Carlson-elliptic₂ [x (not/c (and/c real? negative?))]
                   [y (not/c (and/c real? negative?))]
                   [z (not/c (and/c real? negative?))])
number?
]

Alias for @racket[Rd].

@defproc[
(Carlson-elliptic-2 [x (not/c (and/c real? negative?))]
                    [y (not/c (and/c real? negative?))]
                    [z (not/c (and/c real? negative?))])
number?
]

Alias for @racket[Rd].

@section{Incomplete Elliptic Integrals}

@defproc[
(elliptic-integral-F
 [φ real?]
 [k (and/c real? positive? (</c 1))])
number?
]

Defines the first
@link["http://en.wikipedia.org/wiki/Carlson_symmetric_form#Incomplete_elliptic_integrals"]{incomplete
elliptic integral} via the Carlson symmetric form. Only defined for
@italic{φ ∈ (0, 2π)} and @italic{k²sin² ≤ 1}.

@defproc[
(elliptic-integral-E
 [φ real?]
 [k (and/c real? positive? (</c 1))])
number?
]

The second incomplete elliptic integral.Only defined for @italic{φ ∈
(0, 2π)} and @italic{k²sin² ≤ 1}.

@section{Complete Elliptic Integrals}

These are just the incomplete elliptic integrals with @italic{φ =
1/2}.

@defproc[
(complete-elliptic-integral-K
 [k (and/c real? positive? (</c 1))])
number?
]

Computes the first complete elliptic integral.

@defproc[
(complete-elliptic-integral-E
 [k (and/c real? positive? (</c 1))])
number?
]

Computes the second complete elliptic integral.

@defproc[
(elliptic-integrals
 [k (and/c real? positive? (</c 1))]
 [cc (-> flonum? flonum? any)])
any
]

An older definition of the complete elliptic integrals. @racket[_k] is
the elliptic modulus and @racket[_cc] is a continuation that takes the
values of the first and second complete elliptic integrals. The result
is the result of calling @racket[_cc].

@defproc[
(first-elliptic-integral
 [k (and/c real? positive? (</c 1))])
number?
]

An alias for @racket[complete-elliptic-integral-K] computed using
@racket[elliptic-integrals].

@defproc[
(second-elliptic-integral
 [k (and/c real? positive? (</c 1))])
number?
]

An alias for @racket[complete-elliptic-integral-E] computed using
@racket[elliptic-integrals].

@defproc[
(first-elliptic-integral&derivative
 [k (and/c real? (</c 1))]
 [cont (-> number? number? any)])
[result (k)
 (if (= k 0)
     (cons/c flonum? flonum?)
     any/c)]
]

Computes the first elliptic integral and it's derivative at the
elliptic modulus @racket[_k]. @racket[_cont], the continuation
parameter, is oly applied if @racket[_k] is non-zero.

@section{Jacobi Elliptic Functions}

Defines the
@link["http://en.wikipedia.org/wiki/Jacobi_elliptic_functions"](Jacobi
elliptic functions).

@defproc[
(Jacobi-elliptic-functions
 [u number?]
 [k (and/c real? positive? (</c 1))]
 [cont (-> number? number? number? any)])
any
]

Computes the three Jacobi elliptic functions, @italic{sn},
@italic{cn}, @italic{dn} evaluated at @racket[_u] with elliptic
modulus @racket[_k]. These values are passed to the continuation
@racket[_cont].
