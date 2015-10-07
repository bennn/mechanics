#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [first-elliptic-integral (-> (and/c flonum? positive? (</c 1))
                               flonum?)]
  [second-elliptic-integral (-> (and/c flonum? positive? (</c 1))
                                flonum?)]
  [elliptic-integrals (-> (and/c flonum? positive? (</c 1))
                          (cons/c flonum? flonum?))]))

(require
 (only-in racket/flonum
          fl<
          flabs
          fl/
          fl+
          flsqrt
          fl*
          fl-))

(require (only-in mechanics π π/2 machine-ε))

(define (first-elliptic-integral k)
  (let loop ([a 1.0]
             [b (flsqrt (fl- 1.0
                             (fl* k k)))]
             [c k])
    (if (fl< (flabs c) (* 2.0 machine-ε))
        (fl/ π/2 a)
        (loop (fl/ (fl+ a b) 2.0)
              (flsqrt (fl* a b))
              (fl/ (fl- a b) 2.0)))))

(define (elliptic-integrals k)
  (let loop ([a 1.0]
             [b (flsqrt (fl- 1.0 (fl* k k)))]
             [c k]
             [d 0.0]
             [powers-2 1.0])
    (if (fl< (flabs c) (* 2.0 machine-ε))
        (let ([first-elliptic-integral (fl/ π/2 a)])
          (cons first-elliptic-integral
                (fl* first-elliptic-integral
                     (fl- 1.0 (fl/ d 2.0)))))
        (loop (fl/ (fl+ a b) 2.0)
              (flsqrt (fl* a b))
              (fl/ (fl- a b) 2.0)
              (fl+ d (fl* (fl* c c) powers-2))
              (fl* powers-2 2.0)))))

(define (second-elliptic-integral k)
  (cdr (elliptic-integrals k)))
