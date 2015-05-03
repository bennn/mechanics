#lang racket/base

;; Root finding by successive bisection

;; ---

(require
  mechanics
  (only-in racket/math pi)
)

;; =============================================================================

(define (converge f x0 x1 gen-xm eps)
  (define fx0 (f x0))
  (define fx1 (f x1))
  (define (loop x0 x1 fx0 fx1)
    (cond
      ;[(and iters (zero? iters)) 
      [(= fx0 0.0) x0]
      [(= fx1 0.0) x1]
      [(> (* fx1 fx0) 0.0) (error "root not bounded")]
      [else
        (define xm (gen-xm x0 x1 fx0 fx1))
        (define fxm (f xm))
        (cond
          [(~? x0 x1 #:epsilon eps) xm]
          [(< (* fx1 fxm) 0.0) (loop xm x1 fxm fx1)]
          [else (loop x0 xm fx0 fxm)])]))
  (loop x0 x1 (f x0) (f x1)))

;; --

(define (bisect-2 f x0 x1 eps)
  (define (gen-xm x0 x1 fx0 fx1) (/ (+ x0 x1) 2.0))
  (converge f x0 x1 gen-xm eps))

(define (kepler-2 ecc m)
  (bisect-2
    (lambda (e) (- e (* ecc (sin e)) m))
    0.0
    (* pi 2)
    1e-15))

(define (bisect-fp f x0 x1 eps)
  (define (gen-xm x0 x1 fx0 fx1) (/ (- (* fx1 x0) (* fx0 x1)) (- fx1 fx0)))
  (converge f x0 x1 gen-xm eps))

(define (kepler-fp ecc m)
  (bisect-fp (lambda (e) (- e (* ecc (sin e)) m)) 0.0 (* 2 pi) 1e-15))

(define (bisect f x0 x1 eps #:break [n-break 60])
  (define fuel (box n-break))
  (define (gen-xm x0 x1 fx0 fx1)
    (set-box! fuel (sub1 (unbox fuel)))
    (if (zero? (unbox fuel))
        (/ (- (* fx1 x0) (* fx0 x1)) (- fx1 fx0))
        (/ (+ x0 x1) 2)))
  (converge f x0 x1 gen-xm eps))

(define (kepler ecc m)
  (bisect (lambda (e) (- e (* ecc (sin e)) m)) -.0 (* 2 pi) 1e-15 #:break 20))

;;; If we don't know anything, it is usually a good idea to 
;;;   break the interval into dx-sized pieces and look for 
;;;   roots in each interval.
;;; TODO test
;(define (find-a-root f x0 x1 dx eps continue failure)
;  (define (find x0 x1)
;    (cond
;      [(> (abs (- x0 x1)) dx)
;       (define f0 (f x0))
;       (define f1 (f x1))
;       (cond
;         [(< (* f0 f1) 0) (continue (bisect f x0 x1 eps))]
;         ; (define xm (/ (+ x0 x1) 2))
;         ; TODO what's the case for (find x0 xm) ?
;         [else            (find (/ (+ x0 x1) 2) x1)])]
;      [else failure]))
;  (find x0 x1))
;
;(define (search-for-roots f x0 x1 eps small)
;  (define (find-roots x0 x1)
;    (define f0 (f x0))
;    (define f1 (f x1))
;    (cond
;      [(~? x0 x1 #:epsilon small)
;       (printf "small\n")
;       (if (< (* f0 f1) 0)
;         (list (bisect f x0 x1 eps))
;         '())]
;      [else
;       (define xm (/ (+ x0 x1) 2))
;       (append (find-roots x0 xm) (find-roots xm x1))]))
;  (find-roots x0 x1))

(module+ test
  (require rackunit)

  (check-equal? (kepler-2 .99 .01) 0.34227031649177453)
  (check-equal? (kepler-fp .99 .01) .342270316491775)
  (check-equal? (kepler .99 .01) .3422703164917751)
)
