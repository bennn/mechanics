#lang racket/base

;; Root finding by successive bisection

;; ---

(require
  mechanics
  (only-in racket/math pi)
)

;; =============================================================================

;; --

(define (bisect-2 f x0 x1 eps)
  (define (loop x0 fx0 x1 fx1)
    ;; Possibly-unused variables, depends on the cond
    (define xm (/ (+ x0 x1) 2.0))
    (define fxm (f xm))
    (cond
      [(= fx0 0.0)              x0]
      [(= fx1 0.0)              x1]
      [(> (* fx1 fx0) 0.0)      (error "root not bounded")]
      [(~? x0 x1 #:epsilon eps) xm]
      [(< (* fx1 fxm) 0.0)      (loop xm fxm x1 fx1)]
      [else                     (loop x0 fx0 xm fxm)]))
  (loop x0 (f x0) x1 (f x1)))

(define (kepler-2 ecc m)
  (bisect-2
    (lambda (e) (- e (* ecc (sin e)) m))
    0.0
    (* pi 2)
    1e-15))

(define (bisect-fp f x0 x1 eps)
  (define (loop x0 fx0 x1 fx1)
    (define xm (/ (- (* fx1 x0) (* fx0 x1)) (- fx1 fx0)))
    (define fxm (f xm))
    (cond
      [(= fx0 0.0) x0]
      [(= fx1 0.0) x1]
      [(> (* fx1 fx0) 0.0) (error "root not bounded")]
      [(~? x0 x1 #:epsilon eps) xm]
      [(< (* fx1 fxm) 0.0) (loop xm fxm x1 fx1)]
      [else (loop x0 fx0 xm fxm)]))
  (loop x0 (f x0) x1 (f x1)))

(define (kepler-fp ecc m)
  (bisect-fp (lambda (e) (- e (* ecc (sin e)) m)) 0.0 (* 2 pi) 1e-15))

(define (bisect f x0 x1 eps #:break [n-break 60])
  (define (loop x0 fx0 x1 fx1 iter)
    (define xm
      (if (< iter n-break)
        (/ (+ x0 x1) 2)
        (/ (- (* fx1 x0) (* fx0 x1)) (- fx1 fx0))))
    (define fxm (f xm))
    (cond
      [(= fx0 0.0) x0]
      [(= fx1 0.0) x1]
      [(> (* fx1 fx0) 0.0) (error "root not bounded")]
      [(~? x0 x1 #:epsilon eps) xm]
      [(< (* fx1 fxm) 0.0) (loop xm fxm x1 fx1 (add1 iter))]
      [else (loop x0 fx0 xm fxm (add1 iter))]))
  (loop x0 (f x0) x1 (f x1) 0))

(define (kepler ecc m)
  (bisect (lambda (e) (- e (* ecc (sin e)) m)) -.0 (* 2 pi) 1e-15 #:break 20))

;; If we don't know anything, it is usually a good idea to 
;;   break the interval into dx-sized pieces and look for 
;;   roots in each interval.
(define (find-a-root f x0 x1 dx eps continue failure)
  (define (find x0 x1)
    (cond
      [(> (abs (- x0 x1)) dx)
       (define f0 (f x0))
       (define f1 (f x1))
       (cond
         [(< (* f0 f1) 0) (continue (bisect f x0 x1 eps))]
         ; (define xm (/ (+ x0 x1) 2))
         ; TODO what's the case for (find x0 xm) ?
         [else            (find (/ (+ x0 x1) 2) x1)])]
      [else failure]))
  (find x0 x1))

(define (search-for-roots f x0 x1 eps small)
  (define (find-roots x0 x1)
    (define f0 (f x0))
    (define f1 (f x1))
    (cond
      [(~? x0 x1 #:epsilon small)
       (if (< (* f0 f1) 0)
         (list (bisect f x0 x1 eps))
         '())]
      [else
       (define xm (/ (+ x0 x1) 2))
       (append (find-roots x0 xm) (find-roots xm x1))]))
  (find-roots x0 x1))

(module+ test
  (require rackunit)

  (check-equal? (kepler-2 .99 .01) 0.34227031649177453)
  (check-equal? (kepler-fp .99 .01) .342270316491775)
  (check-equal? (kepler .99 .01) .3422703164917752)
)
