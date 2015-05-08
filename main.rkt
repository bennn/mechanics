#lang racket/base

;; 2015-04-27:
;; Macros for the SICM library.
;; Placed here (as 'main.rkt' in the top directory) for convenience developing.

;; The contents of this file will eventually live somewhere else,
;; and 'main.rkt' will just be the library's API.

(provide
 sign->integer
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

(define (sign->integer x)
  (if (negative? x) -1 1))

;; Sample macro, to make sure package install worked
(define-syntax (def stx)
  (syntax-parse stx
    [(_ x:id e)
     (printf "MACRO WORKED\n")
     #'(define x e)]))
