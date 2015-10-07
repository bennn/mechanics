#lang scribble/manual

@(require
  (for-label
    racket
    mechanics/private/numerics/functions/bessel))

@title{bessel}

@defmodule[mechanics/private/numerics/functions/bessel]

Bessel functions of integer order.

@section{Bessel Functions of the First Kind}

@defproc[ (bessj₀ [z number?]) number? ]

Defines the order 0
@link["http://en.wikipedia.org/wiki/Bessel_function#Bessel_functions_of_the_first_kind:_J.CE.B1"]{Bessel
function of the first kind.}.

@defproc[ (bessj₁ [z number?]) number?]

The order 1 Bessel function of the first kind.

@defproc[
(bessj [n integer?]
       [z number?])
number?
]

Evaluates the order @racket[_n] Bessel function of the first kind at
@racket[_z].

@section{Bessel Functions of the Second Kind}

@defproc[ (bessy₀ [z number?]) number? ]

Defines the order 0
@link["http://en.wikipedia.org/wiki/Bessel_function#Bessel_functions_of_the_second_kind:_Y.CE.B1"]{Bessel
function of the second kind.}.

@defproc[ (bessy₁ [z number?]) number?]

The order 1 Bessel function of the second kind.

@defproc[
(bessy [n integer?]
       [z number?])
number?
]

Evaluates the order @racket[_n] Bessel function of the second kind at
@racket[_z].

@section{Hankel Functions}

@defproc[ (bessh₀ [z number?]) complex? ]

Defines the order 0
@link["http://en.wikipedia.org/wiki/Bessel_function#Hankel_functions:_H.CE.B1.281.29.2C_H.CE.B1.282.29"]{Hankel
function}

@defproc[ (bessh₁ [z number?]) number?]

The order 1 Hankel function. 

@defproc[
(bessh [n integer?]
       [z number?])
number?
]

Evaluates the order @racket[_n] Hankel function at @racket[_z].