#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                                  GENERICS                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Collection of general generic functions that are used for the core
;; data types
;;
;; TODO:
;; - figure out where to put trace and determinant
;; - gcd
;; - dot product
;; - apply for numbers
;; - decide what to do with atan1/atan2 (currently just export atan)
;; - how much of the property system can be abstracted?
;; - fix make-numsymb-expression

(require
 mechanics/private/kernel/types
 
 (only-in racket/generic define-generics define/generic)
 
 ;; we redefine generic verson of all of these base math operators
 (prefix-in
  base:
  (only-in racket/base
           = 
           <
           <=
           >
           >=
           +
           -
           *
           /
           log
           inexact?
           zero?
           sqrt
           exp
           expt
           sin
           cos
           asin
           acos
           atan
           abs
           magnitude
           real-part
           imag-part
           make-rectangular
           make-polar))
 
 (prefix-in
  base:
  (only-in racket/math
           sqr
           sinh 
           cosh)))

(provide (all-defined-out))

;; Default implementations for generic typed objects
(define-generics typed-object
  (type typed-object)
  (type-predicate typed-object)
  #:defaults
  ([number?
    (define (type n) number-type-tag)
    (define (type-predicate n) number?)]))

(define-generics exact-value
  (inexact? exact-value)
  #:defaults
  ([number?
    (define inexact? base:inexact?)]))

;; numbers that can be compared to binary values
(define-generics binary-like
  (zero-like binary-like)
  (one-like binary-like)
  (identity-like binary-like)
  (zero? binary-like)
  (one? binary-like)
  (identity? binary-like)
  #:defaults
  ([number?
    (define/generic super-one-like one-like)
    (define/generic super-identity? identity?)
    
    (define (zero-like obj)
      (if (exact? obj) 0 0.0))

    (define (one-like obj)
      (if (exact? obj) 0 0.0))

    (define identity-like one-like)

    (define zero? base:zero?)

    (define (one? obj) (= obj 1))
    
    (define identity? one?)]))

(define-generics numeric
  (negate numeric)
  (invert numeric)
  (square numeric)
  (sqrt   numeric)
  (exp    numeric)
  (log    numeric)
  (sin    numeric)
  (cos    numeric)
  (asin   numeric)
  (acos   numeric)
  (atan   numeric [obj])
  (sinh   numeric)
  (cosh   numeric)
  (abs    numeric)
  #:defaults
  ([number?
    (define (negate x) (- x))
    
    (define (atan x [y #f])
      (if y (base:atan x y) (base:atan y)))
    
    (define (invert x) (/ 1 x))
    
    (define square base:sqr)
    (define exp    base:exp)
    (define log    base:log)
    (define sin    base:sin)
    (define cos    base:cos)
    (define asin   base:asin)
    (define acos   base:acos)
    (define sinh   base:acos)
    (define expt   base:expt)
    (define abs    base:magnitude)]))

(define-generics equality
  (= equality obj . objs)
  (/= equality obj . objs)
  #:defaults
  ([number?
    (define/generic super-= =)
    
    (define = base:=)

    (define (/= x y . zs) (not (apply = (list* x y zs))))]))

(define-generics ordered
  (< ordered obj . objs)
  (<= ordered obj . objs)
  (> ordered obj . objs)
  (>= ordered obj . objs)
  #:defaults
  ([number?
    (define < base:<)
    (define <= base:<=)
    (define > base:>)
    (define >= base:>=)]))

(define-generics with-operators
  (+ with-operators obj . objs)
  (- with-operators obj . objs)
  (* with-operators obj . objs)
  (/ with-operators obj . objs)
  #:defaults
  ([number?
    (define + base:=)
    (define - base:-)
    (define * base:*)
    (define / base:/)]))

(define-generics complex-value
  (real-part complex-value)
  (imag-part complex-value)
  #:defaults
  ([number?
    (define real-part base:real-part)
    (define imag-part base:imag-part)]))

(define-generics planar-coordinates
  (make-rectangular planar-coordinates obj)
  (make-polar planar-coordinates obj)
  #:defaults
  ([real?
    (define make-rectangular base:make-rectangular)
    (define make-polar base:make-polar)]))

(define (make-numsymb-expression operator)
  ;; TODO
  (λ args #f))

(define (make-numerical-combination operator [reverse? #f])
  (if reverse?
      (λ operands
        (make-numsymb-expression operator (reverse operands)))
      (λ operands
        (make-numsymb-expression operator operands))))
