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
  (or (and (positive? a) (or (zero? b) (negative? b)))
      (and (positive? b) (or (zero? a) (negative? a)))))

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


;;; If we are provided with a derivative of f as well as f
;;;  we can try to converge faster with Newton's method.
(define (newton-with-false-position-search f&df x1 x2 #:epsilon [eps 1e-15])
  (define (fps1 x1 fx1 dfx1 x2 fx2 dfx2)
    (define (try x)
      (if (opposite-sign? (- x x1) (- x x2))
        (next x)
        (next (/ (- (* x2 fx1) (* fx2 x1)) (- fx1 fx2)))))
    (define (next x)
      (f&df x (lambda (fx dfx)
                (if (opposite-sign? fx1 fx)
                  (fps1 x1 fx1 dfx1 x fx dfx)
                  (fps1 x fx dfx x2 fx2 dfx2)))))
    (define afx1 (abs fx1))
    (define afx2 (abs fx2))
    (define (best-guess)
      (if (opposite-sign? fx1 fx2)
        (average x1 x2)
        (if (< afx1 afx2) x1 x2)))
    (cond
      [(< afx1 eps)
       (if (< afx2 eps) (best-guess) x1)]
      [(< afx2 eps)
       x2]
      [(~? x1 x2 #:epsilon eps)
       (best-guess)]
      [(< afx1 afx2)
       (try (- x1 (/ fx1 dfx1)))]
      [else
       (try (- x2 (/ fx2 dfx2)))]))
  (f&df x1 (lambda (fx1 dfx1)
             (f&df x2 (lambda (fx2 dfx2)
                        (fps1 x1 fx1 dfx1 x2 fx2 dfx2))))))


;; =============================================================================

(module+ test
  (require rackunit)

  (check-equal?
    (false-position-search (lambda (e) (- e (* .99 (sin e)) .01)) 0.0 (* 2 pi))
    .342270316491775)

  (check-equal?
    (newton-with-false-position-search (lambda (e cont) (cont (- e (* .99 (sin e)) .01) (- 1 (* .99 (cos e))))) 0.0 (* 2 pi))
    .34227031649177553)
)
