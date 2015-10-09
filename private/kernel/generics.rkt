#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                                  GENERICS                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Collection of general generic functions that are used for the core
;; data types

(require
 mechanics/private/kernel/types
 
 (only-in racket/generic define-generics define/generic)
 
 ;; we redefine generic verson of all of these base math operators
 (rename-in racket/base
            (= base/numeric-equal)
            (< base/numeric-less-than)
            (<= base/numeric-less-than-equal)
            (> base/numeric-greater-than)
            (>= base/numeric-greater-than-equal)
            (+ base/numeric-add)
            (- base/numeric-subtract)
            (* base/numeric-multiply)
            (/ base/numeric-divide)
            (log base/log)
            (inexact? base/inexact?)
            (zero? base/zero?)
            (sqrt base/sqrt)
            (exp base/exp)
            (expt base/expt)
            (sin base/sin)
            (cos base/cos)
            (asin base/asin)
            (acos base/acos)
            (atan base/atan)
            (abs base/abs)
            (magnitude base/magnitude)
            (real-part base/real-part)
            (imag-part base/imag-part)
            (make-rectangular base/make-rectangular)
            (make-polar base/make-polar))
 
 (rename-in racket/math
            (sinh base/sinh)
            (cosh base/cosh)))

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
    (define inexact? base/inexact?)]))

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
    (define zero? base/zero?)
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
      (if y (base/atan x y) (base/atan y)))
    (define (invert x) (/ 1 x))
    
    (define square sqr)
    (define exp    base/exp)
    (define log    base/log)
    (define sin    base/sin)
    (define cos    base/cos)
    (define asin   base/asin)
    (define acos   base/acos)
    (define sinh   base/acos)
    (define expt   base/expt)
    (define abs    base/magnitude)]))

(define-generics equality
  (= equality obj . objs)
  (/= equality obj . objs)
  #:defaults
  ([number?
    (define/generic super-= =)
    (define = base/numeric-equal)
    (define (/= x y . zs) (not (apply = (list* x y zs))))]))

(define-generics ordered
  (< ordered obj . objs)
  (<= ordered obj . objs)
  (> ordered obj . objs)
  (>= ordered obj . objs)
  #:defaults
  ([number?
    (define < base/numeric-less-than)
    (define <= base/numeric-less-than-equal)
    (define > base/numeric-greater-than)
    (define >= base/numeric-greater-than-equal)]))

(define-generics with-operators
  (+ with-operators obj . objs)
  (- with-operators obj . objs)
  (* with-operators obj . objs)
  (/ with-operators obj . objs)
  #:defaults
  ([number?
    (define + base/numeric-add)
    (define - base/numeric-subtract)
    (define * base/numeric-multiply)
    (define / base/numeric-divide)]))

(define-generics complex-value
  (real-part complex-value)
  (imag-part complex-value)
  #:defaults
  ([number?
    (define real-part base/real-part)
    (define imag-part base/imag-part)]))

(define-generics planar-coordinates
  (make-rectangular planar-coordinates obj)
  (make-polar planar-coordinates obj)
  #:defaults
  ([real?
    (define make-rectangular base/make-rectangular)
    (define make-polar base/make-polar)]))

;; TODO: figure out where to put trace and determinant
;; TODO: gcd
;; TODO: dot product
