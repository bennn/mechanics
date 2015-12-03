#lang mechanics

;; Vector functions missing from racket & racket/math

;; Alternatively, use the `math/flonum` vector operations

(require mechanics)
(provide/api
  vector-zero?
  #:contract (-> (vectorof number?) boolean?)
  #:doc "True if all components of the vector are `zero?`"

  vector=
  #:contract (->* [] [] #:rest (vectorof any/c) boolean?)
  #:doc "True if all argument vectors are `equal?`"

  vector+
  #:contract (->* [] [] #:rest (vectorof number?) (vectorof number?))
  #:doc "Sum vectors componentwise"

  vector-
  #:contract (->* [] [] #:rest (vectorof number?) (vectorof number?))
  #:doc "Take the difference of vectors, componentwise"

  vector-negate
  #:contract (-> (vectorof number?) (vectorof number?))
  #:doc "Pointwise negation"

  vector-scalar*
  #:contract (-> number? (vectorof number?) (vectorof number?))
  #:doc "Multiply by a scalar"

  vector-scalar/
  #:contract (-> number? (vectorof number?) (vectorof number?))
  #:doc "Divide by a scalar"

  vector-dot-product
  #:contract (->* [] [] #:rest (vectorof number?) number?)
  #:doc "Generalized dot product"

  vector-square
  #:contract (-> (vectorof number?) number?)
  #:doc "Sugar for (vector-dot-product v v)"

  vector-cube
  #:contract (-> (vectorof number?) number?)
  #:doc "Sugar for (vector-dot-product v v v)"

  vector-euclidean-norm
  #:contract (-> (vectorof number?) number?)
  #:doc "Take the Euclidean norm of a vector (the length of the arrow the vector represents)"

  vector-complex-norm
  #:contract (-> (vectorof number?) number?)
  #:doc ""

  vector-empty?
  #:contract (-> (vectorof any/c) boolean?)
  #:doc "True if the argument is an empty vector"

  vector-maxnorm
  #:contract (-> (vectorof number?) number?)
  #:doc "Return the max value in a non-empty vector"

  vector-make-unit
  #:contract (-> (vectorof number?) (vectorof number?))
  #:doc "Normalize a vector (make a similar vector with Euclidean norm of 1)"

  vector-unit?
  #:contract (-> (vectorof number?) boolean?)
  #:doc "True if the argument has a Euclidean norm of 1"

  vector-conjugate
  #:contract (-> (vectorof number?) (vectorof number?))
  #:doc "Compute the pointwise complex conjugate of a vector"

  vector-cross-product
  #:contract (-> (vector/c number? number? number?) (vector/c number? number? number?) (vector/c number? number? number?))
  #:doc "Compute a vector perpendicular to the pair of 3D vectors"

  vector-inexact?
  #:contract (-> (vectorof number?) boolean?)
  #:doc "True if any component of the vector is an inexact number"
)

;; -----------------------------------------------------------------------------

(require
  racket/vector
  (only-in racket/match match-define)
  (only-in racket/math conjugate)
)

;; =============================================================================
;; === Syntax & Assertions

(define-syntax-rule (empty-vector-error location)
  (raise-user-error location "Undefined for empty vector"))

(define-syntax-rule (dimension-error location . vec*)
  (raise-user-error location "Undefined for vectors '~a' of unequal dimension"))

(define (assert-nonempty vec #:src location)
  (when (vector-empty? vec)
    (empty-vector-error location)))

(define (assert-dimension location N . vec*)
  (for ([vec (in-list vec*)])
    (unless (= N (vector-length vec))
      (raise-user-error location (format "Expected ~a-dimensional vector, got ~a" N vec)))))

;; =============================================================================
;; === API

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

(define (vector- . vec*)
  (apply vector-map (cons - vec*)))

(define (vector-negate vec)
  (vector-map - vec))

(define (vector-scalar* s vec)
  (vector-map (lambda (i) (* s i)) vec))

(define (vector-scalar/ s vec)
  (vector-scalar* (/ 1 s) vec))

(define (vector-dot-product . vec*)
  (cond
   [(eq? '() vec*) 0]
   [else
    (for/fold ([acc 0])
              ([i (in-range (vector-length (car vec*)))])
      (+ acc (apply * (map (lambda (v) (vector-ref v i)) vec*))))]))

(define (vector-square vec)
  (vector-dot-product vec vec))

(define (vector-cube vec)
  (vector-dot-product vec vec vec))

(define (vector-euclidean-norm vec)
  (sqrt (vector-square vec)))

(define (vector-inner-product vec1 vec2)
  (for/fold ([acc 0])
            ([v1 (in-vector vec1)]
             [v2 (in-vector vec2)])
    (+ acc (* (conjugate v1) v2))))

(define (vector-complex-norm vec)
  (sqrt (vector-inner-product vec vec)))

(define (vector-empty? vec)
  (zero? (vector-length vec)))

;; Largest number in vector
(define (vector-maxnorm vec)
  (assert-nonempty vec #:src 'vector-maxnorm)
  (for/fold ([acc (vector-ref vec 0)])
            ([v (in-vector vec)])
    (max acc v)))

(define (vector-make-unit vec)
  (vector-scalar/ (vector-euclidean-norm vec) vec))

(define (vector-unit? vec)
  (= 1 (vector-square vec)))

(define (vector-conjugate vec)
  (vector-map conjugate vec))

;; Cross product of independent 3d vectors is perpendicular to both
(define (vector-cross-product vec1 vec2)
  (assert-dimension 'vector-cross-product 3 vec1 vec2)
  (match-define (vector v0 v1 v2) vec1)
  (match-define (vector w0 w1 w2) vec2)
  (vector (- (* v1 w2) (* v2 w1))
          (- (* v2 w0) (* v0 w2))
          (- (* v0 w1) (* v1 w0))))

(define (vector-apply vec arg*)
  (vector-map (lambda (v) (apply v arg*)) vec))

;;bg; scmutils also defines 'vector-arity', for taking the
;;    joint arity of all functions in the vector

(define (vector-inexact? vec)
  (for/or ([v (in-vector vec)]) (inexact? v)))

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
    [(vector 1 2 3) == (vector 1 2 3)]
    [(vector 1) (vector 1) (vector 1) == (vector 3)]
    [(vector 1 2 3) (vector 2 5 8) == (vector 3 7 11)]
  )

  (check-vectorop vector-
    [(vector 5 5 5) (vector 0 0 0) == (vector 5 5 5)]
    [(vector) (vector) == (vector)]
    [(vector 1 1 1 1) (vector 1 1 1 1) == (vector 0 0 0 0)]
    [(vector 8 6 7 5) (vector 1 2 8 6) (vector 1 0 0 1) == (vector 6 4 -1 -2)]
  )

  (check-vectorop vector-negate
    [(vector 1 2 3) == (vector -1 -2 -3)]
    [(vector 0) == (vector 0)]
    [(vector -2 2) == (vector 2 -2)]
  )

  (check-vectorop vector-scalar*
    [0 (vector 1 9 2 3) == (vector 0 0 0 0)]
    [8 (vector 1 2 3) == (vector 8 16 24)]
    [3 (vector 1) == (vector 3)]
  )

  (check-vectorop vector-scalar/
    [1 (vector 1 1 1) == (vector 1 1 1)]
    [3 (vector 1 2 3) == (vector 1/3 2/3 1)]
    [1/4 (vector 2 8 1.2) == (vector 8 32 4.8)]
  )

  (check-vectorop vector-dot-product
    [(vector 1 1 1) (vector 1 1 1) == 3]
    [(vector 2 8 5) (vector 4 12 2) == (+ 8 96 10)]
    [(vector) (vector) == 0]
    [(vector 1 1 1) == 3]
    [(vector 1) (vector 1) (vector 1) == 1]
    [(vector 32 1) (vector 6 5) (vector 8 13) == (+ (* 32 48) 65)]
  )

  (check-vectorop vector-square
    [(vector 1 2) == 5]
    [(vector 8 2 6 1/5) == (+ 64 4 36 1/25)]
    [(vector) == 0]
  )

  (check-vectorop vector-cube
    [(vector 2 2 2) == 24]
    [(vector 1/3 1/4 1/5) == (+ 1/27 1/64 1/125)]
    [(vector) == 0]
    [(vector 1 1 1 1) == 4]
  )

  (check-vectorop vector-euclidean-norm
    [(vector) == 0]
    [(vector 1) == 1]
    [(vector 99) == 99]
    [(vector 4 4 4 4) == 8]
  )

  (check-vectorop vector-inner-product
    [(vector 2) (vector 2) == 4]
    [(vector 1/4 2/7) (vector 3 6/7) == (+ 3/4 12/49)]
    [(vector 1.0+2.0i -5+2i) (vector 1+2.0i -5+2i) == (+ (* 1.0-2.0i 1.0+2.0i) (* -5-2i -5+2i))]
  )

  (check-vectorop vector-complex-norm
    [(vector) == 0]
    [(vector 2+2i) == (sqrt (* 2-2i 2+2i))]
    ;; -- TODO add more tests
  )

  (check-vectorop vector-empty?
    [(vector) == #t]
    [(vector 1) == #f]
    [(vector 3.14 .00159) == #f]
  )

  (check-vectorop vector-maxnorm
    [(vector 1) == 1]
    [(vector 1/3 1/4 1/5) == 1/3]
    [(vector -6 -3 -1) == -1]
  )

  (check-vectorop vector-make-unit
    [(vector 1) == (vector 1)]
    [(vector 4) == (vector 1)]
    [(vector 8) == (vector 1)]
    [(vector 3 4) == (vector 3/5 4/5)]
    [(vector 4 4 4 4) == (vector 1/2 1/2 1/2 1/2)]
  )

  (check-vectorop vector-unit?
    ;; -- #t
    [(vector 1) == #t]
    [(vector 1/2 1/2 1/2 1/2) == #t]
    [(vector 1/9 2/9 7/9 5/9 1/9 1/9) == #t]
    ;; -- #f
    [(vector) == #f]
    [(vector 8) == #f]
    [(vector -2 -1 3) == #f]
    [(vector 1/2 1/2 1/2) == #f]
  )

  (check-vectorop vector-conjugate
    [(vector) == (vector)]
    [(vector 2 18 4) == (vector 2 18 4)]
    [(vector 2.5 1/5 8.2) == (vector 2.5 1/5 8.2)]
    [(vector 1+3i) == (vector 1-3i)]
    [(vector 1+6/7i 8-1.2i) == (vector 1-6/7i 8+1.2i)]
  )

  (check-vectorop vector-cross-product
    [(vector 1 1 1) (vector 1 1 1) == (vector 0 0 0)]
    [(vector 2 2 2) (vector -2 -2 2) == (vector 8 -8 0)]
    [(vector 5 0 0) (vector 0 5 0) == (vector 0 0 25)]
  )

  (check-vectorop vector-inexact?
    ;; -- #t
    [(vector 1.0) == #t]
    [(vector 0 0 0 0 3.14) == #t]
    ;; -- #f
    [(vector) == #f]
    [(vector 1 23) == #f]
    [(vector 1/4 2/3) == #f]
  )

)
