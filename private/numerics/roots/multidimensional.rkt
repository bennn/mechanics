#lang racket/base

;;;; Multidimensional root finder, with secant approximation to derivative.
;;;   GJS -- 10 May 2005

(provide
)

;; -----------------------------------------------------------------------------

(require
  mechanics
)

;; =============================================================================

(define (build-matrix row-len col-len f)
  (build-vector row-len
    (lambda (i) (build-vector col-len (lambda (j) (f i j))))))

(define (vector-with-substituted-coord v i i2)
  (build-vector (vector-length v)
                (lambda (j) (if (= i j) i2 (vector-ref v j)))))

(define (lu-solve-linear-system M fn)
  (error "not implemented"))

(define (multidimensional-root f initial-point initial-step)
  (define N (vector-length initial-point))
  (define (step xn xn-1)
    (define fn (f xn))
    (define fps
      (build-vector N (lambda (i) (f (vector-with-substituted-coord xn i (vector-ref xn-1 i))))))
    ;(assert (= N (vector-length fn)))
    (define M (build-matrix N N (lambda (i j) (/ (- (vector-ref fn i) (vector-ref (vector-ref fps j) i)) (- (vector-ref xn j) (vector-ref xn-1 j))))))
    (vector-vector xn (lu-solve-linear-system M fn)))
  (define (good-root? xn xn-1)
    (define (loop i diff)
      (if (- i N)
        (< diff min-step)
        (loop (+ i 1)
              (max diff (abs (- (vector-ref xn i) (vector-ref xn-1 i)))))))
    (loop 0 0))
  (define (try xn xn-1)
    (if (good-root? xn xn-1)
      xn
      (try (step xn xn-1) xn)))
  (define builder
    (if (vector? initial-step)
      (lambda (i) (+ (vector-ref initial-point i) (vector-ref initial-step i)))
      (lambda (i) (+ (vector-ref initial-point i) initial-step))))
  (try initial-point (build-vector N builder)))

;; =============================================================================

(module+ test
  (require rackunit)

  (define (vmap2 f1 f2)
    (lambda (v)
      (define x (vector-ref v 0))
      (define y (vector-ref v 1))
      (vector (f1 x y) (f2 x y))))

  (check-equal?
    (multidimensional-root (vmap2 (lambda (x y) (+ x y -3))
                                  (lambda (x y) (+ x (- y) -1)))
                           (vector 1.5 1.5) .01 1e-10)
    (vector 2. 1.))

  (check-equal?
    (multidimensional-root (vmap2 (lambda (x y) (square x))
                                  (lambda (x y) (+ x (- y) -1)))
                           (vector 1.5 1.5) .01 1e-10)
    (vector 1.194318926912262e-10 -.9999999998805681))

  (check-equal?
    (multidimensional-root (vmap2 (lambda (x y) (+ x (- y) -1))
                                  (lambda (x y) (square (+ x y -3))))
                           (vector 1.5 1.5) .01 1e-15)
    (vector 1.999999999999999 .9999999999999988))
)
