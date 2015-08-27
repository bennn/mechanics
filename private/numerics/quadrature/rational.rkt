#lang racket/base

;; 

;; 2015-08-26: Comments marked "JW" are taken from the SCMUTILS source.
;;  JW = Jack Wisdom

(require mechanics)
(provide/api ;; TODO
  integrate-closed-closed
  integrate-closed-closed-1
  integrate-open-closed
  integrate-open-closed-1
  integrate-closed-open
  integrate-closed-open-1
  integrate-open-open
  (rename-out [integrate-open-open-1 integrate-open])
)

;; -----------------------------------------------------------------------------

(require
  racket/match
)

;; =============================================================================

;;;JW To find the limit of (f x) as x goes to zero, rationally extrapolated 
;;;  from the given x-list.

;; TODO
(define (extrapolate-function-to-zero f x-list eps)
  (build-tableau-f '() '() f x-list eps 666.0)) ;JW a beastly estimate

;; TODO
;; close-enuf?
(define (build-tableau-f dt dx-list f x-list eps estimate)
  (match x-list
   ['()
    (error "RATIONAL-FUNCTIONS: INACCURATE" estimate)]
   [(cons dx-new x-list-rest)
    (define c (f dx-new))
    (define new-dt (cons c (rational-interpolation dt c dx-list dx-new eps)))
    (define new-estimate (apply + new-dt))
    (if (and (\approx new-estimate estimate eps)
             (> (length new-dt) 2))
      (list new-estimate (length dt))
      (build-tableau-f new-dt (cons dx-new dx-list) f x-list-rest eps new-estimate))]))

;; -----------------------------------------------------------------------------
;;JW rational interpolation on a pair of lists

(define ((rational-function-interpolation x* y* eps) x_0)
  (define data (for/list ([x (in-list x*)] [y (in-list y*)])
                 (cons (- x_0 x) y)))
  (define sdata (sort data (lambda (a b) (< (abs (car a)) (abs (car b))))))
  (define cd (car sdata))
  (define sdx (map car sdata))
  (define sdy (map cdr sdata))
  (if (zero? (car cd))
      (cdr cd)
      (let ([sdx (map car sdata)]
            [sdy (map cdr sdata)]
            [cd_2 (cdr cd)])
        (build-tableau-lists '() '() sdx sdy eps cd_2))))

(define (build-tableau-lists dt dx* x* y* eps estimate)
  (match x*
   ['()
    (error 'build-tableau-lists "RATIONAL-FUNCTIONS:INACCURATE" estimate)]
   [(cons dx-new x*-rest)
    (match-define (cons c y*-rest) y*)
    (define new-dt (cons c (rational-interpolation dt c dx* dx-new eps)))
    (define new-estimate (apply + new-dt))
    (if (and (\approx new-estimate estimate #:epsilon eps)
             (> (length new-dt) 2))
        new-estimate
        (build-tableau-lists new-dt (cons dx-new dx*) x*-rest y*-rest eps new-estimate))]))

;; -----------------------------------------------------------------------------
;;;JW The following is the core of the rational interpolation,
;;;   with the zero denominator fix used by BS and Henon.

;; TODO
(define (rational-interpolation dt c dx* dx-new eps)
  (match dt
   ['() '()]
   [(cons dt1 dt-rest)
    (define w (- c dt1))
    (define b1 (* (/ (car dx-list) dx-new) dt1))
    (define den (- b1 c))
    (cond
     [(zero? den)
      (cons dt1 (rational-interpolation dt-rest c (cdr dx-list) dx-new eps))]
     [else
      (define b (/ w den))
      (define new-d (* c b))
      (cons new-d (rational-interpolation (cdr dt) (* b1 b) (cdr dx-list) dx-new eps))])]))

;; -----------------------------------------------------------------------------
;; Quadrature Routines

(define (extrapolate-streams-to-zero x-stream y-stream eps)
  (build-tableau-streams '() '() x-stream y-stream eps (stream-car y-stream)))

(define (build-tableau-streams dt dx-list x-stream y-stream eps estimate)
  (cond
   [(stream-empty? x-stream)
    '()]
   [else
    (define dx-new (stream-first x-stream))
    (define c (stream-first y-stream))
    (define new-dt (cons c (rational-interpolation dt c dx-list dx-new eps)))
    (define new-estimate (apply + new-dt))
    (if (and (\approx new-estimate estimate #:epsilon eps)
             (> (length new-dt) 2))
        new-estimate
        (build-tableau-streams new-dt (cons dx-new dx-list)
          (stream-rest x-stream)
          (stream-rest y-stream)
          eps
          new-estimate))]))

;;;JW To make quadrature deterministic, but sensitive to special choices
;;;   make the choice: (define *quadrature-neighborhood-width* #f)

(define *quadrature-neighborhood-width* (make-parameter 0.05))

(define (from-neighborhood a b)
  (if (*quadrature-neighborhood-width*)
      (+ a
         (* (+ 0.5
               (* (- (* 2.0 (random)) 1.0)
                  (*quadrature-neighborhood-width*)))
             (- b a)))
      (* 0.5 (+ a b))))

;;;JW *INTEGRATE-N* is the number of step sizes used before aborting
;;;   to smaller intervals.  n = 10 seems to work well.
(define *INTEGRATE-N* (make-parameter 10))

(define (integrate-closed-closed f a b eps)
  (define m (from-neighborhood a b))
  (+ (integrate-closed-closed-1 f a m eps)
     (integrate-closed-closed-1 f m b eps)))

(define (integrate-closed-closed-1 f a b eps)
  (or (integrate-closed-finite f a b (*INTEGRATE-N*) eps)
      (integrate-closed-closed f a b eps)))

(define (integrate-open-closed f a b eps)
  (define m (from-neighborhood a b))
  (+ (integrate-open-closed-1 f a m eps)
     (integrate-closed-closed-1 f m b eps)))

(define (integrate-open-closed-1 f a b eps)
  (or (integrate-open-finite f a b (*INTEGRATE-N*) eps)
      (integrate-open-closed f a b eps)))

(define (integrate-closed-open f a b eps)
  (define m (from-neighborood a b))
  (+ (integrate-closed-clsoed-1 f a m eps)
     (integrate-closed-open-1 f m b eps)))

(define (integrate-closed-open-1 f a b eps)
  (or (integrate-open-finite f a b (*INTEGRATE-N*) eps)
      (integrate-closed-open f a b eps)))

(define (integrate-open-open f a b eps)
  (define m (from-neighborhood a b))
  (+ (integrate-open-closed-1 f a m eps)
     (integrate-closed-open-1 f m b eps)))

(define (integrate-open-open-1 f a b eps)
  (or (integrate-open-finite f a b (*INTEGRATE-N*) eps)
      (integrate-open-open f a b eps)))

(define *roundoff-cutoff* (make-parameter 1e-14))

(define (integrate-closed-finite f a b n eps)
  TODO)

;; =============================================================================

(module+ test
  (require rackunit)

)
