#lang racket/base

(provide
  ;; (->* ((-> Number Number) Number Number) (#:epsilon Number #:iters Number) (List Boolean Number Natural))
  ;; Finds roots of function `f` in domain from `x0` to `x1` provided
  ;;  `f(x0) X f(x1) <= 0`.
  ;; Optional arguments `#:epsilon` and `#:iters` set the tolerance for
  ;;  floating point error and maximum number of iterations, respectively.
  ;; Returns a list of three elements:
  ;; - The first element is boolean indicating whether the algorithm converged
  ;; - The second is the root, or #f
  ;; - The third is the number of iterations required.
  ;; The number of function evaluations is always
  ;;  two more than the number of iterations.
  ;; (From Numerical Recipes, 2nd Edition, Press et al.)
  zbrent
)

;; -----------------------------------------------------------------------------

(require
  mechanics
)

;; =============================================================================

(define *machine-epsilon*
  (let loop ((e 1.0))
     (if (= 1.0 (+ e 1.0))
         (* 2 e)
         (loop (/ e 2)))))

(define (zbrent f x0 x1 #:epsilon [eps 1e-15] #:iters [imax 100])
  (define fx0 (f x0))
  (define fx1 (f x1))
  (when (or (and (> fx0 0) (> fx1 0)) (and (< fx0 0) (< fx1 0)))
    (error "Root must be bracketed in zbrent [~a , ~a]" x0 x1))
  (define (loop iter x0 fx0 x1 fx1 x2 fx2 d e)
    (cond
      [(= iter imax)
       (list #f x1 iter)]
      [(or (and (> fx1 0) (> fx2 0)) (and (< fx1 0) (< fx2 0)))
       (define u (- x1 x0))
       (loop iter x0 fx0 x1 fx1 x0 fx0 u u)]
      [(< (abs fx2) (abs fx1))
       (loop iter x1 fx1 x2 fx2 x0 fx0 d e)]
      [else
       (define tol1 (+ (* *machine-epsilon* (abs x1))
                       (/ eps 2.0)))
       (define xm (/ (- x2 x1) 2.0))
       (define (next1 p0 q0)
         (define p (abs p0))
         (define q (if (> p 0.0) (- q0) q0))
         (define min1 (- (* 3.0 xm q) (abs (* tol1  q))))
         (define min2 (abs (* e q)))
         (if (< (* 2.0 p) (min min1 min2))
           (next2 d (/ p q))
           (next2 d xm)))
       (define (next2 e d)
         (define new-x1
           (if (> (abs d) tol1)
             (+ x1 d)
             (+ x1 (if (> xm 0.0) (abs tol1) (- (abs tol1))))))
         (loop (add1 iter) x1 fx1 new-x1 (f new-x1) x2 fx2 d e))
       (cond
         [(or (<= (abs xm) tol1) (= fx1 0.0))
          (list #t x1 iter)]
         [(and (>= (abs e) tol1) (> (abs fx0) (abs fx1)))
          (define s (/ fx1 fx0))
          (if (= x0 x2)
            (next1 (* 2.0 xm s) (- 1.0 s))
            (let ([q (/ fx0 fx2)]
                  [r (/ fx1 fx2)])
              (next1 (* s (- (* 2.0 xm q (- q r))
                             (* (- x1 x0) (- r 1.0))))
                     (* (sub1 q) (sub1 r) (sub1 s)))))]
         [else (next2 d xm)])]))
  (loop 0 x0 fx0 x1 fx1 x0 fx0 (- x1 x0) (- x1 x0)))

;; =============================================================================

(module+ test
  (require rackunit)

  ;; Call zbrent, tracking the number of times `f` is invoked
  (define (zbrent+numcalls f x0 x1)
    (define num-calls 0)
    (define (f* x)
      (set! num-calls (add1 num-calls))
      (f x))
    (define res (zbrent f* x0 x1))
    (cons num-calls res))

  (check-equal?
    (zbrent+numcalls (lambda (x) (- (sin x) 0.5)) 0. 1.5)
    (cons 21 (list #t .5235987755982988 19)))
  (check-equal?
    (zbrent (lambda (x) (- (sin x) 0.5)) 0. 1.5)
    (list #t .5235987755982988 19))

  (check-equal?
    (zbrent+numcalls (lambda (x) (+ (* 2 x (exp (- 3))) 1 (* -2 (exp (* -1 3 x))))) 0. 1.)
    (cons 22 (list #t .223705457654663 20)))

  (check-equal?
    (zbrent+numcalls (lambda (x) (- (* (add1 (expt (- 1 10) 2)) x) (expt (- 1 (* 10 x)) 2))) 0. 1.)
    (cons 20 (list #t 9.9000099980005e-3 18)))

  (check-equal?
    (zbrent+numcalls (lambda (x) (- (square x) (expt (- 1 x) 5))) 0 1.)
    (cons 21 (list #t .345954815848242 19)))

  (check-equal?
    (zbrent+numcalls (lambda (x) (+ (expt x 19) 1e-4)) -3. 5.)
    (cons 31 (list #t -0.6158482110660264 29)))

  (check-equal?
    (zbrent+numcalls (lambda (x) (cube x)) -1. 10.)
    (cons 86 (list #t -8.096477227045762e-19 84)))

  (check-equal?
    (zbrent (lambda (x) (expt x 9)) -1. 10.)
    (list #t -1.7948404394016643e-16 78))

  (check-equal?
    (zbrent (lambda (x) (expt x 19)) -1. 10. #:iters 200)
    (list #t 3.4764349675327277e-16 80))

)
