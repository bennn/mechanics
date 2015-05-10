#lang racket/base

;; 2015-04-27:
;; Macros for the SICM library.
;; Placed here (as 'main.rkt' in the top directory) for convenience developing.

;; The contents of this file will eventually live somewhere else,
;; and 'main.rkt' will just be the library's API.
(require racket/contract)

(provide
 def
 (contract-out
  [π flonum?]
  [π/2 flonum?]
  [π/3 flonum?]
  [π/4 flonum?]
  [π/6 flonum?]
  [π/12 flonum?]
  [2/π flonum?]
  [3π/4 flonum?]
  [machine-ε flonum?])
 ;replace 'provide' with a form that requires a doc and a contract
 ;macros for easy list access (to replace vector-ref, etc)
)

;; -----------------------------------------------------------------------------

(require
 (for-syntax
  racket/base
  syntax/parse))

(require (only-in racket/math pi))

;; =============================================================================

;; Sample macro, to make sure package install worked
(define-syntax (def stx)
  (syntax-parse stx
    [(_ x:id e)
     (printf "MACRO WORKED\n")
     #'(define x e)]))

;; =============================================================================
;;
;; Constants

(define π pi)
(define π/2 (/ π 2))
(define π/3 (/ π 3))
(define π/4 (/ π 4))
(define π/6 (/ π 6))
(define π/12 (/ π 12))
(define 2/π (/ 2 π))
(define 3π/4 (* 3 π/4))

(define (machine-ε)
  (define (find-machine-ε ε)
    (if (= 1.0 (+ ε 1.0))
        (* 2 ε)
        (find-machine-ε (/ ε 2))))
  (define the-machine-ε #f)
  (unless the-machine-ε
    (set! the-machine-ε (find-machine-ε 1.0)))
  the-machine-ε)

(define *machine-epsilon* machine-ε)
