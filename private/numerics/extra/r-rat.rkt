#lang racket/base

;; Does Racket's math library already provide this?
;; I'm not sure we need this file.

(provide
 )

;; -----------------------------------------------------------------------------

(require mechanics)

;; =============================================================================

(define (exact-rational-approximation x
                                      #:epsilon [eps 1e-15]
                                      #:max-denominator [maxden 6e23])
  (define (rat1 x cont) ;; (-> Rational (-> Numerator Denominator Exact-Rational) Exact-Rational)
    (define ix (inexact->exact x))
    (let loop ([num ix] [den 1] [onum 1] [oden 0] [xx x] [a ix])
      (cond
       [(> den maxden) #f]
       [(and (not (zero? den)) (< (abs (/ (- x (/ num den)) x)) eps))
        (cont num den)]
       [else
        (define y (/ 1 (- xx a)))
        (define iy (inexact->exact y))
        (loop (+ (* iy num) onum)
              (+ (* iy den) oden)
              num
              den
              y
              iy)])))
  (cond
   [(exact? x) x]
   [(< (abs x) eps) 0]
   [(< x 0) (rat1 (abs x) (lambda (num den) (/ (- num) den)))] ;; write this prettier?
   [else (rat1 x /)]))

;;; Some processes, such as finding the roots of a polynomial, can
;;; benefit by heuristic rounding of results (to a nearby rational).

;;; Heuristic rounding will occur to a rational within
(define heuristic-rounding-tolerance 1.0e-9)
;;; that is expressible with a denominator less than the
(define heuristic-rounding-denominator 100)
;;; if such a rational exists.

(define (heuristic-round-real x)
  (define r (exact-rational-approximation x heuristic-rounding-tolerance heuristic-rounding-denominator))
  ;; Why exact->inexact?
  (if r (exact->inexact r) x))

(define (heuristic-round-complex z)
  (cond
   [(real? z) (heuristic-round-real z)]
   [else
    (define r (real-part z))
    (define i (imag-part z))
    (define ar (abs r))
    (define ai (abs i))
    (cond
     [(and (< ar heuristic-rounding-tiny) (< ai heuristic-rounding-tiny))
      0.]
     [(< ai (* heuristic-rounding-irrelevant ar))
      (heuristic-round-real r)]
     [(< ar (* heuristic-rounding-irrelevant ai))
      (make-rectangular 0 (heuristic-round-real i))]
     [else
      (define ag (/ (angle z) pi))
      (define af (heuristic-round-real ag))
      (if (= ag af)
          (make-rectangular (heuristic-round-real r)
                            (heuristic-round-real i))
          (make-polar (heuristic-round-real (magnitude z))
                      (* pi af)))])]))

(define heuristic-rounding-irrelevant 1.0e-9)
(define heuristic-rounding-tiny 1.0e-15)

(define (heuristic-round-angle a)
  (define ag (/ a pi))
  (define af (heuristic-round-real ag))
  (if (= ag af) a (* pi af)))

;; =============================================================================

(module+ test
  (require rackunit)
  ;; TODO
)
