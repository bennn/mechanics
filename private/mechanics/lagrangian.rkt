#lang racket/base

;; 2015-07-31
;;  Built following the textbook, not the code

(require mechanics)
(provide/api
)

;; -----------------------------------------------------------------------------

(require
  (only-in mechanics/private/kernel/vector
    vector-dot-product)
  (only-in mechanics/private/numerics/quadrature
    definite-integral)
)

;; =============================================================================
;; TODO these don't belong here

(define up list)
(define Gamma (lambda (x) (error "Gamma")))

;; =============================================================================
;; === 1.4

;; Data Definition: local tuple
(struct local (
  time ;; Natural
  coordinates ;; (Vectorof ???)
  velocity ;; (Vectorof ???)
  higher-derivatives ;; (Listof ???)
) #:transparent)

;; L(t, x, v) = 1/2 m v^2
(define ((L-free-particle mass) lcl)
  (define v (local-velocity lcl))
  (* 1/2 mass (vector-dot-product v v)))

;; q, in the text
(define (coordinate-path time->x time->y time->z)
  (lambda (t)
    (up (time->x t)
        (time->y t)
        (time->z t))))

(define (Lagrangian-action L q t1 t2)
  (definite-integral (compose L (Gamma q)) t1 t2))

;; =============================================================================

(module+ test
  (require rackunit)

)
