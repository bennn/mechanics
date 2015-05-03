#lang racket/base

;; Secant method for finding roots

(provide
  ;; (->* ((-> Number Number) #:init Number #:limit Number) (#:epsilon Number #:valepsilon Number #:zeroepsilon Number #:iters Natural) Number)
  ;; Find root using the secant method
  secant-method
)

;; -----------------------------------------------------------------------------

(require
  mechanics
)

;; =============================================================================

(define (secant-method f #:init guess1 #:limit guess2 #:epsilon [eps 1e-15] #:valepsilon [valepsilon 1e-30] #:zeroepsilon [zeroepsilon 1e-30] #:iters [maxiters #f])
  (define (secant-iteration pk-1 fpk-1 pk fpk count)
    (cond
      [(~? pk-1 pk #:epsilon eps) pk]
      [(< (magnitude fpk) valepsilon) pk]
      [(and maxiters (> count maxiters)) (error "maxiter: ~a ~a ~a ~a ~a" pk-1 fpk-1 pk fpk count)]
      [(< (magnitude (- fpk-1 fpk)) zeroepsilon)
       (define pk+1 (/ (+ pk pk-1) 2))
       (define fpk+1 (f pk+1))
       (secant-iteration pk fpk pk+1 fpk+1 (add1 count))]
      [else
        (define pk+1 (- pk (* (- pk pk-1) (/ fpk (- fpk fpk-1)))))
        (define fpk+1 (f pk+1))
        (secant-iteration pk fpk pk+1 fpk+1 (add1 count))]))
  (secant-iteration guess1 (f guess1) guess2 (f guess2) 0))

;; =============================================================================

(module+ test
  (require rackunit)

  (check-equal?
    (secant-method cos #:init -1. #:limit 2.)
    1.5707963267948966)

  (check-equal?
    (secant-method cube #:init -1. #:limit 2.)
    -9.344250202828578e-11)

  (check-equal?
    (secant-method cube #:init 1. #:limit 2.)
    9.273573199032124e-11)

  (check-equal?
    (secant-method square #:init -1. #:limit 3. #:epsilon 1e-17)
    -6.35401500343362e-16)

  (check-equal?
    (secant-method square #:init -1. #:limit 2. #:epsilon 1e-17)
    0.)

  (check-equal?
    (secant-method square #:init 1. #:limit 2. #:epsilon 1e-17)
    6.854008160388252e-16)
)

