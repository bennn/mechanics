#lang racket/base

;; In simple cases, if one knows the function and its derivative,
;;  Newton's method is a quick-and-dirty way to find a root.
;;  However, one must start close to the root to get it to converge.
;; Kahan's trick gives faster convergence in slow cases, but will
;;  slow down the search for simple roots.
;; See http://www.cs.berkeley.edu/~wkahan/Math128/RealRoots.pdf

(provide
  ;; (->* (Number #:init Number) (#:epsilon Number) Number)
  ;; Use Newton's method to find a root.
  newton-search
  ;; (->* (Number #:init Number #:limit Number) (#:epsilon Number) Number)
  ;; Newton's method 'optimized' for long-running cases by Kahan's trick.
  newton-kahan-search
)

;; -----------------------------------------------------------------------------

(require
  mechanics
  (only-in math/statistics mean)
)

;; =============================================================================

(define (newton-search f&df #:init x0 #:epsilon [eps 1e-15])
  (define (newton-improve xn)
    (f&df xn (lambda (fn dfn) (- xn (/ fn dfn)))))
  (define (loop xn)
    (define xn+1 (newton-improve xn))
    (if (~? xn xn+1 #:epsilon eps)
      (average xn xn+1)
      (loop xn+1)))
  (loop x0))

(define (newton-kahan-search f&df #:init x0 #:limit x1 #:epsilon [eps 1e-15])
  (define (kahan-trick x)
    (round (abs x)))
  (define (psi x) (f&df x /))
  (define (secant-improve xn psn xn-1 psn-1)
    (- xn (* psn (kahan-trick (/ (- xn xn-1) (- psn psn-1))))))
  (define (loop xn psn xn-1 psn-1)
    (if (~? xn xn-1 #:epsilon eps)
      (average xn xn-1)
      (let ([xn+1 (secant-improve xn psn xn-1 psn-1)])
        (loop xn+1 (psi xn+1) xn psn))))
  (loop x1 (psi x1) x0 (psi x0)))

;; =============================================================================

(module+ test
  (require rackunit)

  (check-equal?
    (newton-search (lambda (x cont) (cont (cos x) (- (sin x)))) #:init 1.0 #:epsilon 1e-15)
    1.5707963267948966)
  (check-equal?
    (newton-search (lambda (x cont) (cont (cos x) (- (sin x)))) #:init 1.0)
    1.5707963267948966)

  ;; If the root is multiple, the convergence is much slower 
  ;;  and much less accurate.
  (check-equal?
    (newton-search (lambda (x cont) (cont (- 1.0 (sin x)) (- (cos x)))) #:init 1)
    1.570796319310356)

  ;; Kahan's hack speeds up search for multiple roots, but costs
  ;;  a bit for simple roots.
  (check-equal?
    (newton-kahan-search (lambda (x cont) (cont (cos x) (- (sin x)))) #:init 1.0 #:limit 2.0)
    1.5707963267948966)

  (check-equal?
    (newton-kahan-search (lambda (x cont) (cont (- 1.0 (sin x)) (- (cos x)))) #:init 1.0 #:limit 2.0)
     1.5707963255702555)

  ;;bg: No noticeable speedup for me, the check fails.
  ;;    Guess we should count iterations.
  ;(define (newton)
  ;  (newton-search (lambda (x cont) (cont (- 1.0 (sin x)) (- (cos x)))) #:init 0))
  ;(define (kahan)
  ;  (newton-kahan-search (lambda (x cont) (cont (- 1.0 (sin x)) (- (cos x)))) #:init .0 #:limit 2.0))

  ;(define (time-thunk thk)
  ;  (define-values (res cpu-time-ms real-time gc-time) (time-apply thk '()))
  ;  cpu-time-ms)

  ;(check-true (< (time-thunk kahan) (time-thunk newton)))
)
