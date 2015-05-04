#lang racket/base

;; 2015-04-27:
;; Macros for the SICM library.
;; Placed here (as 'main.rkt' in the top directory) for convenience developing.

;; The contents of this file will eventually live somewhere else,
;; and 'main.rkt' will just be the library's API.

(provide
 average
 square
 cube
 ~?
 def
 ;replace 'provide' with a form that requires a doc and a contract
 ;macros for easy list access (to replace vector-ref, etc)
)

;; -----------------------------------------------------------------------------

(require
 (for-syntax
  racket/base
  syntax/parse))

;; =============================================================================

(define (square x)
  (expt x 2))

(define (cube x)
  (expt x 3))

(define (average . nums)
  (define N (length nums))
  (for/sum ([n nums]) (/ n N)))

;; Equal up to some epsilon
(define (~? n1 n2 #:epsilon e)
  (< (abs (- n1 n2)) e))

;; Sample macro, to make sure package install worked
(define-syntax (def stx)
  (syntax-parse stx
    [(_ x:id e)
     (printf "MACRO WORKED\n")
     #'(define x e)]))

(module+ test
  (require rackunit)
  (check-equal? (average 1 2 3) 2)
)
