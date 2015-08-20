#lang racket/base

;; 

(require mechanics)
(provide/api
  integrate-closed-closed
  integrate-closed-closed-1
  integrate-open-closed
  integrate-open-closed-1
  integrate-closed-open
  integrate-closed-open-1
  integrate-open-open
  integrate-open-open-1
)

;; -----------------------------------------------------------------------------

(require
  racket/match
)

;; =============================================================================

;; TODO
(define (extrapolate-function-to-zero f x-list eps)
  (build-tableau-f '() '() f x-list eps 666.0)) ;JW a beastly estimate

;; TODO
(define (build-tableau-f dt dx-list f x-list eps estimate)
  (match x-list
   ['()
    (error "RATIONAL-FUNCTIONS: INACCURATE" estimate)]
   [(cons dx-new x-list-rest)
    (define c (f dx-new))
    (define new-dt (cons c (rational-interpolation dt c dx-list dx-new eps)))
    (define new-estimate (sum-list-flo new-dt))
    (if (and (close-enuf? new-estimate estimate eps)
             (> (length new-dt) 2))
      (list new-estimate (length dt))
      (build-tableau-f new-dt (cons dx-new dx-list) f x-list-rest eps new-estimate))]))

;; TODO
(define (rational-interpolation dt c dx-list dx-new eps)
  (match dt
   ['() '()]
   [(cons dt1 dt-rest)
    (define w (- c dt1))
    (define b1 (* (/ (car dx-list) dx-new) dt1))
    (define den (- b1 c))
    (cond
     [(= den 0.0)
      (cons dt1 (rational-interpolation dt-rest c (cdr dx-list) dx-new eps))]
     [else
      (define b (/ w den))
      (define new-d (* c b))
      (cons new-d (rational-interpolation (cdr dt) (* b1 b) (cdr dx-list) dx-new eps))])]))

;; =============================================================================

(module+ test
  (require rackunit)

)
