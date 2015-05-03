#lang racket/base

(provide
)

;; -----------------------------------------------------------------------------

(require
  mechanics
  (only-in racket/math pi)
)

;; =============================================================================

(define (opposite-sign? a b)
  (or (and (< a 0) (>= b 0))
      (and (< b 0) (>= a 0))))

;; To find x such that f(x)=0, 
;;  given x1, x2 where f(x1) and f(x2) have opposite sign.
(define (false-position-search f x1 x2 #:epsilon [eps 1e-15])
  (define (fps1 x1 fx1 x2 fx2)
    (define afx1 (abs fx1))
    (define afx2 (abs fx2))
    (define (best-guess)
      (if (opposite-sign? fx1 fx2)
        (average x1 x2)
        (if (< afx1 afx2) x1 x2)))
    (if (< afx2 eps)
      x2
      (if (~? x1 x2 #:epsilon eps)
        (best-guess)
        (let* ([x (/ (- (* x2 fx1) (* fx2 x1))
                     (- fx1 fx2))]
               [fx (f x)])
          (if (opposite-sign? fx1 fx)
            (fps1 x1 fx1 x fx)
            (fps1 x fx x2 fx2))))))
  (fps1 x1 (f x1) x2 (f x2)))


;; =============================================================================

(module+ test
  (require rackunit)

  (check-equal?
    (false-position-search (lambda (e) (- e (* .99 (sin e)) .01)) 0.0 (* 2 pi))
    .342270316491775)
)
