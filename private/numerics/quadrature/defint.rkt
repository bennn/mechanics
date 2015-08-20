#lang racket/base

;; 

(require mechanics)
(provide/api
  definite-integral
  #:contract (->* [(-> TODO) TODO TODO] [#:epsilon TODO #:compile? Boolean] TODO)
  #:doc "TODO"

  compile-integrand
  #:contract (parameter/c boolean?)
  #:doc "TODO"
)

;; -----------------------------------------------------------------------------

(require
)

;; =============================================================================

(define *compile-integrand?* (make-parameter #f))

(define (definite-integral f t1 t2
                           #:epsilon [eps machine-epsilon]
                           #:compile? [compile? (compile-integrand?)])
  (cond
    [(and (number? t1) (number? t2) (= t1 t2))
     0]
    [(not compile?)
     ((make-definite-integrator f t1 t2 eps) 'integral)]
    [else
     ((make-definite-integrator (compile-procedure f) t1 t2 eps) 'integral)]))

;; TODO quadrature.rkt

;; =============================================================================

(module+ test
  (require rackunit)
)

