#lang scribble/manual

@(require (for-label racket))

@title{mechanics}

This is a library for doing classical mechanics. It is a direct port
of the
@link["http://groups.csail.mit.edu/mac/users/gjs/6946/linux-install.htm"]{scmutils}
library intended for use in reading
@link["http://groups.csail.mit.edu/mac/users/gjs/6946/sicm-html/"]{Structure
and Interpretation of Classical Mechanics} by Sussman and Wisdom.

@local-table-of-contents[]

@include-section["numerics/functions.scrbl"]
@include-section["numerics/integer.scrbl"]
