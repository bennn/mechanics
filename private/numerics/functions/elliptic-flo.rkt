#lang racket/base

(require racket/flonum)
(require racket/math)

(define (first-elliptic-integral k)
  (if (fl< k 1.0)
      (let loop ([a 1.0]
                 [b (flsqrt (fl- 1.0
                                 (fl* k k)))]
                 [c k])
        (if (fl< (flabs c) (* 2.0 machine-ε))
            (fl/ π/2 a)
            (loop (fl/ (fl+ a b) 2.0)
                  (flsqrt (fl* a b))
                  (fl/ (fl- a b) 2))))
      (error "first-elliptic-integral k >= 1" k)))

(define (elliptic-integrals k)
  (let loop ([a 1.0]
             [b (flsqrt (fl- 1.0 (fl* k k)))]
             [c k]
             [d 0]
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

;; TODO: import these from constants file
(define machine-ε
  (let loop ([ε 1.0])
    (if (= 1.0 (+ ε 1.0))
        (* 2 ε)
        (loop (/ ε 2)))))
(define π pi)
(define π/2 (/ π 2))
