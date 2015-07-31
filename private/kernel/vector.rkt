#lang racket/base

;; Vector functions missing from racket & racket/math

;; Alternatively, use the `math/flonum` vector operations

(require mechanics)
(provide/api
  vector-zero?
  #:contract (-> (vector/c number?) boolean?)
  #:doc "True if all components of the vector are `zero?`"

  vector=
  #:contract (->* [] [] #:rest (vector/c any/c) boolean?)
  #:doc "True if all argument vectors are `equal?`"

  vector+
  #:contract (->* [] [] #:rest (vector/c number?) (vector/c number?))
  #:doc "Sum vectors componentwise"

;  vector-
;  #:contract (->* [] [] #:rest (vector/c number?) (vector/c number?))
;  #:doc "Take the difference of vectors, componentwise"

;  dot-product
;  #:contract
;  #:doc
)

;; -----------------------------------------------------------------------------

(require
  racket/vector
)

;; =============================================================================

;;bg; Extra definition
;(define (vector-elementwise f . vec*)
;  (apply vector-map (cons f vec*)))

(define (vector-zero? vec)
  (for/and ([v (in-vector vec)])
    (zero? v)))

(define (vector= . vec*)
  (or (eq? '() vec*)
      (and
        (for/fold ([prev (car vec*)])
                  ([vec  (cdr vec*)])
          (and prev
               (equal? prev vec)
               vec))
        #t)))

(define (vector+ . vec*)
  (apply vector-map (cons + vec*)))


;; =============================================================================

(module+ test
  (require rackunit)

  (define-syntax-rule (check-vectorop op [v* ... == v3] ...)
    (begin (check-equal? (op v* ...) v3) ...))

  (check-vectorop vector=
    ;; -- pass
    [         == #t]
    [(vector) == #t]
    [(vector 0) (vector 0) (vector 0) == #t]
    [(vector 1 2 3 4) (vector 1 2 3 4) == #t]
    [(vector 1/2) (vector 1/2) == #t]
    ;; -- fail
    [(vector 1) (vector 2) == #f]
    [(vector) (vector 66.6) == #f]
  )

  (check-vectorop vector+
    ;; -- pass
    [(vector 1 2 3) == (vector 1 2 3)]
    [(vector 1) (vector 1) (vector 1) == (vector 3)]
    [(vector 1 2 3) (vector 2 5 8) == (vector 3 7 11)]
    ;; -- fail TODO
  )
)
