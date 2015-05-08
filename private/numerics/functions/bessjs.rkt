#lang racket/base

;;;; Bessel Functions:

;;; Jn+1(x) = 2n/x Jn(x) - Jn-1(x)   ------   unstable!

;;;   But Jn-1(x) = 2n/x Jn(x) - Jn+1(x) is stable, so use Miller trick.

;;;  (bessjs n x) returns a list
;;;    ( J0(x) ... Jn(x) ) 
;;;   that is good to machine precision for x < 2 and large n.

(define (bessjs nmax x)
  (let loop ([n (round-to-even (+ nmax (sqrt (* bessjs-accur (+ nmax 3)))))]
             [ans '()]
             [miller-sum 0.0]
             [Jn+1 1.0]
             [Jn 1.0])
    (define (next)
      (- (/ (* 2 n Jn) x) Jn+1))
    (define (ms)
      (if (even? n) (+ Jn miller-sum) miller-sum))
    (when (> Jn bessjs-bigno)
      (begin
        (set! ans (map (λ (x)
                         (* x bessjs-bigni))
                       ans))
        (set! miller-sum (* miller-sum bessjs-bigni))
        (set! Jn (* Jn bessjs-bigni))
        (set! Jn+1 (* Jn+1 bessjs-bigni))))
    (cond
     [(= n 0)
      (let ([miller-sum (+ Jn (* 2 miller-sum))])
        (map (λ (x) (/ x miller-sum))
             (cons Jn ans)))]
     [(<= n nmax)
      (let ([Jn-1 (next)])
        (loop (- n 1)
              (cons Jn ans)
              (ms)
              Jn
              Jn-1))]
     [else
      (loop (- n 1) ans (ms) Jn (next))])))


(define bessjs-bigno (exact->inexact (expt 2 36)))
(define bessjs-bigni (exact->inexact (expt 2 -36)))
(define bessjs-accur 50)

(define (round-to-even x)
  (let ((xn (inexact->exact (round x))))
    (if (odd? xn)
	(+ xn 1)
	xn)))
