#lang info

(define collection "mechanics")
(define deps '("base"))
(define build-deps
  '("cover"
    "cover-coveralls"
    "racket-doc"
    "rackunit-lib"
    "scribble-lib"
    ))
(define pkg-desc "Library for SICM")
(define pkg-authors '(ben ben))
(define version "0.0")
(define raco-commands '(
  ("mechanics"
   (submod mechanics main)
   "open a REPL with the mechanics library"
   #f)
))
(define scribblings '(("scribblings/mechanics.scrbl" (multi-page))))
