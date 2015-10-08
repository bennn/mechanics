#lang info

;; 2015-04-28:
;; Not sure exactly what we need here, but these settings
;; seem like a good guess

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
(define scribblings '(("scribblings/mechanics.scrbl" (multi-page))))
