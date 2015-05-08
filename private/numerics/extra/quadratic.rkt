#lang racket/base

;; From Hamming, roots of quadratic without bad roundoff.
;; a*x^2 + b*x + c = 0

(provide
 )

;; -----------------------------------------------------------------------------

(require
 )

;; =============================================================================

(define (sign->integer x)
  (if (negative? x) -1 1))

(define (quadratic a b c
                   ;; continuations for each case
                   two-roots
                   [complex-roots #f]
                   [double-root #f]
                   [linear #f]
                   [no-solution #f])
  (if (zero? a)
      (if (zero? b)
          (if (procedure? no-solution)
              (no-solution a b c)
              (error "No solution -- QUADRATIC" a b c))
          (if (procedure? linear)
              (linear (/ (- c) b))
              (error "Not QUADRATIC" a b c)))
      (let ([d (- (* b b) (* 4 a c))])
        (if (zero? d)
            (let ([root (/ b (* -2 a))])
              (if (procedure? double-root)
                  (double-root root)
                  (two-roots root root)))
            (let ([q (* -1/2 (+ b (* (sign->integer b) (sqrt d))))])
              (let ([r1 (/ q a)] [r2 (/ c q)])
                (if (or (> d 0)
                        (not (procedure? complex-roots)))
                    (two-roots r1 r2)
                    (complex-roots r1 r2))))))))

  ;; =============================================================================

(module+ test
  (require rackunit)

  (define (test-quadratic a b c)
    (quadratic a b c
      (lambda (r1 r2) `(two-roots ,r1 ,r2))
      (lambda (r1 r2) `(complex-roots ,r1 ,r2))
      (lambda (r) `(double-root ,r))
      (lambda (r) `(linear ,r))
      (lambda (a b c) `(no-solution ,a ,b ,c))))

  (check-equal? (test-quadratic 1 0 2)
                '(complex-roots -1.4142135623730951i
                                +1.414213562373095i))

  (check-equal? (test-quadratic 2 -14 20)
                '(two-roots 5 2))

  (check-equal? (test-quadratic 0 1 2)
                '(linear -2))

  (check-equal? (test-quadratic 1 -2 1)
               '(double-root 1))

  (check-equal? (test-quadratic 0 0 0)
               '(no-solution 0 0 0))

  (check-equal? (test-quadratic 0 0 1)
               '(no-solution 0 0 1))

  (check-equal? (test-quadratic 0 1 0)
               '(linear 0))

  (check-equal? (test-quadratic 1 0 0)
               '(double-root 0))

  (check-equal? (test-quadratic 2 10 100)
               '(complex-roots -5/2-6.614378277661476i
                               -2.5+6.614378277661477i))
  )
