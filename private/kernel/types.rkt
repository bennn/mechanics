#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                                   TYPES                                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Definiions of the core data types of the mechanics library.
;;
;; Types Defined Here:
;; - number
;; - vector
;; - up
;; - down
;; - matrix
;; - function
;;
;; There are also a collection of predicates to test for membership in
;; the given types.

(require
 (only-in racket/fixnum fx=))

(define (make-type type-tag
                   abstract-type-tag
		   quantity-predicate
                   concrete-predicate
                   abstract-predicate)
  (list type-tag
        abstract-type-tag
	quantity-predicate
        concrete-predicate
        abstract-predicate))

(define (type-tag type)
  (car type))

(define (abstract-type-tag type)
  (cadr type))

(define (quantity-predicate type)
  (caddr type))

(define (concrete-predicate type)
  (cadddr type))

(define (abstract-predicate type)
  (car (cddddr type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                                 TYPE TAGS                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define number-type-tag            '*number*)
(define with-units-type-tag        '*with-units*)
(define unit-type-tag              '*unit*)
(define vector-type-tag            '*vector*)
(define abstract-vector-type-tag   '*vector*)
(define quaternion-type-tag        '*quaternion*)
(define column-type-tag            '*vector*)
(define abstract-column-type-tag   '*vector*)
(define row-type-tag               '*down*)
(define abstract-row-type-tag      '*abstract-row*)
(define matrix-type-tag            '*matrix*)
(define abstract-matrix-type-tag   '*abstract-matrix*)
(define function-type-tag          '*function*)
(define abstract-function-type-tag '*function*)
(define differential-type-tag      '*diff*)
(define operator-type-tag          '*operator*)
(define series-type-tag            '*series*)

(define type-tags
  (list number-type-tag
	unit-type-tag
	with-units-type-tag
	vector-type-tag
	quaternion-type-tag
	row-type-tag
	abstract-row-type-tag
	matrix-type-tag
	abstract-matrix-type-tag
	function-type-tag
	differential-type-tag
	operator-type-tag
	series-type-tag))

(define compound-type-tags
  (list vector-type-tag
	quaternion-type-tag
	row-type-tag
	matrix-type-tag
	series-type-tag
	abstract-matrix-type-tag))

(define abstract-type-tags
  (list number-type-tag
	vector-type-tag
	abstract-row-type-tag
	abstract-matrix-type-tag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                                 PREDICATES                                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (abstract-quantity? x)
  (memq (g:type x) abstract-type-tags))

(define (abstract-number? x)
  (or (literal-number? x)
      (symbol? x)))

(define (literal-number? x)
  (and (pair? x)
       (eq? (car x) number-type-tag)))

(define (literal-real? x)
  (and (literal-number? x)
       ((has-property? 'real) x)))

(define (numerical-quantity? x)
  (or (number? x)
      (abstract-number? x)
      (and (differential? x)
	   (numerical-quantity? (differential-of x)))
      (and (with-units? x)
	   (numerical-quantity? (u:value x)))))

(define (with-units? x)
  (and (pair? x)
       (eq? (car x) with-units-type-tag)))

(define (units? x)
  (or (eq? x '&unitless)
      (and (pair? x)
	   (eq? (car x) unit-type-tag))))

(define (compound-type-tag? x)
  ;; Will need to add tensors, etc.
  (memq x compound-type-tags))

(define (not-compound? x)
  (not (or (vector? x)
	   (and (pair? x)
		(compound-type-tag? (car x))))))

(define (scalar? x)
  (not (or (vector? x)
	   (and (pair? x)
		(compound-type-tag? (car x)))
	   (function? x))))

(define (abstract-vector? x)
  (and (pair? x)
       (eq? (car x) vector-type-tag)))

(define (vector-quantity? v)
  (or (vector? v)
      (abstract-vector? v)
      (and (differential? v)
	   (vector-quantity? (differential-of v)))))

(define (quaternion? v)
  (and (pair? v)
       (eq? (car v) quaternion-type-tag)))

(define (quaternion-quantity? v)
  (quaternion? v))


(define (column? x)
  (vector? x))

(define up? column?)

(define (abstract-column? x)
  (and (pair? x) (eq? (car x) abstract-column-type-tag)))

(define (column-quantity? v)
  (or (column? v)
      (abstract-column? v)
      (and (differential? v)
	   (column-quantity? (differential-of v)))))

(define (row? x)
  (and (pair? x)
       (eq? (car x) row-type-tag)))

(define down? row?)

(define (abstract-row? x)
  (and (pair? x) (eq? (car x) abstract-row-type-tag)))

(define (row-quantity? v)
  (or (row? v)
      (abstract-row? v)
      (and (differential? v)
	   (row-quantity? (differential-of v)))))

(define (structure? x)
  (or (column? x) (row? x)))


(define (abstract-structure? x)
  (or (abstract-column? x) (abstract-row? x)))

(define (matrix? m)		
  (and (pair? m)
       (eq? (car m) matrix-type-tag)))

(define (matrix-quantity? m)
  (or (matrix? m)
      (abstract-matrix? m)
      (and (differential? m)
	   (matrix-quantity? (differential-of m)))))

(define (abstract-matrix? m)
  (and (pair? m)
       (eq? (car m) abstract-matrix-type-tag)))

(define (square-matrix? matrix)
  (and (matrix? matrix)
       (fx= (caadr matrix) (cdadr matrix))))

(define (square-abstract-matrix? matrix)
  (and (pair? matrix)
       (eq? (car matrix) abstract-matrix-type-tag)
       ((has-property? 'square) matrix)))

(define (operator? x)
  (and (apply-hook? x)
       (eq? (car (apply-hook-extra x))
	    operator-type-tag)))

(define (not-operator? x)
  (not (operator? x)))

(define (function-quantity? f)
  (procedure? f))			;apply hooks are procedures.

(define (function? f)
  (and (procedure? f)
       (not (operator? f))))

(define (cofunction? f)			;may be combined with a function
  (not (operator? f)))

(define (abstract-function? f)
  (and (typed-or-abstract-function? f)
       (f:expression f)))

(define (typed-function? f)
  (and (typed-or-abstract-function? f)
       (not (f:expression f))))

(define (typed-or-abstract-function? f)
  (and (apply-hook? f)
       (eq? (car (apply-hook-extra f))
	    function-type-tag)))

(define (differential? obj)
  (and (pair? obj)
       (eq? (car obj) differential-type-tag)))

(define (not-differential? obj)
  (not (differential? obj)))

(define (series? s)
  (and (pair? s)
       (eq? (car s) series-type-tag)))

(define (not-series? s)
  (not (series? s)))

(define (not-differential-or-compound? x)
  (not (or (vector? x)
	   (and (pair? x)
		(or (compound-type-tag? (car x))
		    (eq? (car x) differential-type-tag))))))

(define (not-d-c-u? x)
  (not (or (eq? x '&unitless)
	   (vector? x)
	   (and (pair? x)
		(or (compound-type-tag? (car x))
		    (eq? (car x) differential-type-tag)
		    (eq? (car x) with-units-type-tag)
		    (eq? (car x) unit-type-tag))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                            TYPE INSTANTIATIONS                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *number*
  (make-type '*number* '*number* numerical-quantity? number? abstract-number?))

(define *vector*
  (make-type vector-type-tag
	     abstract-vector-type-tag
	     vector-quantity?
             vector?
             abstract-vector?))

(define *up*
  (make-type column-type-tag
	     abstract-column-type-tag
	     vector-quantity?
             column?
             abstract-column?))

(define *down*
  (make-type row-type-tag
             abstract-row-type-tag
             row-quantity?
             row?
             abstract-row?))

(define *matrix*
  (make-type matrix-type-tag
	     abstract-matrix-type-tag
	     matrix-quantity?
             matrix?
             abstract-matrix?))

(define *function*
  (make-type function-type-tag
	     abstract-function-type-tag
	     function-quantity?
             function?
             abstract-function?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                              HELPER FUNCTIONS                              ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (g:type obj)
  (error "TODO: port this from generics"))

(define (u:value obj)
  (error "TODO: port this from generics"))

(define (f:expression obj)
  (error "TODO: port this from generics"))

(define (has-property? p)
  (error "TODO: port this from generics"))

(define (apply-hook? obj)
  (error "TODO: figure out what apply hooks are"))

(define (apply-hook-extra obj)
  (error "TODO: figure out what apply hooks are"))

(define (differential-of x)
  (error "TODO: port diff"))
