#lang racket/base

;; Consult "Numerical Computation of Real or Complex Elliptic
;; Integrals" Carlson (1994) http://arxiv.org/pdf/math/9409227v1.pdf
;;
;; The algorithm in scmutils seems similar to what is in that paper,
;; but slightly different.
;;
;; Consider also exporting under the more familiar names Rf, Rj, Rc,
;; Rd. Is there any input method that gets those names as subscripts.
(require racket/contract/base)
(provide
 (contract-out
  ;; Carlson elliptic integrals R_F
  [Rf three-nonnegative-reals->number]
  [Carlson-elliptic₁ three-nonnegative-reals->number]
  [Carlson-elliptic-1 three-nonnegative-reals->number]
  [Carlson-elliptic₁-simple three-nonnegative-reals->number]
  [Carlson-elliptic-1-simple three-nonnegative-reals->number]

  ;; R_D
  [Rd three-nonnegative-reals->number]
  [Carlson-elliptic₂ three-nonnegative-reals->number]
  [Carlson-elliptic-2 three-nonnegative-reals->number]

  ;; Elliptic integrals of the first kind: F(φ,k)

  ;; TODO: does it make sense to allow k²sin²φ > 1? NO
  ;;       first param should be in [0, 2π]
  [elliptic-integral-F (-> real?
                           (and/c real? positive? (</c 1))
                           number?)]

  [elliptic-integral-E (-> real?
                           (and/c real? positive? (</c 1))
                           number?)]
  
  [complete-elliptic-integral-K (-> (and/c real? positive? (</c 1))
                                    number?)]
  
  [complete-elliptic-integral-E (-> (and/c real? positive? (</c 1))
                                    number?)]
  
  [elliptic-integrals (-> (and/c real? positive? (</c 1))
                          (-> flonum? flonum? any)
                          any)]
  
  [first-elliptic-integral (-> (and/c real? positive? (</c 1))
                               number?)]
  
  [second-elliptic-integral (-> (and/c real? positive? (</c 1))
                                number?)]
  
  [first-elliptic-integral&derivative (->i ([k (and/c real? (</c 1))]
                                            [cont (-> number? number? any)])
                                           [result (k)
                                                   (if (= k 0)
                                                       (cons/c flonum? flonum?)
                                                       any/c)])]
  
  [Jacobi-elliptic-functions (-> number?
                                 (and/c real? positive? (</c 1))
                                 (-> number? number? number? any)
                                 any)]))

(define nonnegative-real? (not/c (and/c real? negative?)))

(define three-nonnegative-reals->number
  (-> nonnegative-real?
      nonnegative-real?
      nonnegative-real?
      number?))

(require
 (only-in mechanics π π/2 square machine-ε))

(require
 (only-in racket/math cosh tanh))

(require
 (only-in racket/fixnum fx< fx+))

(define (Rf x y z)
  (define ε (expt machine-ε (/ 1 6)))
  (define C₁ (/ 1. 24.))
  (define C₂ 0.1)
  (define C₃ (/ 3. 44.))
  (define C₄ (/ 1. 14.))
  (define √x (sqrt x))
  (define √y (sqrt y))
  (define √z (sqrt z))
  (define λₐ (+ (* √x
                   (+ √y √z))
                (* √y √z)))
  (define xp (* .25 (+ x λₐ)))
  (define yp (* .25 (+ y λₐ)))
  (define zp (* .25 (+ z λₐ)))
  (define μ (/ (+ xp yp zp) 3.))
  (define Δx (/ (- μ x) μ))
  (define Δy (/ (- μ y) μ))
  (define Δz (/ (- μ z) μ))
  (define error (max (abs Δx) (abs Δy) (abs Δz)))

  (if (> error ε)
      (Rf xp yp zp)
      (let ([e₂ (- (* Δx Δy)
                   (* Δz Δz))]
            [e₃ (* Δx Δy Δz)])
        (/ (+ 1
              (* (- (* C₁ e₂)
                    (+ C₂ (* C₃ e₃)))
                 e₂)
              (* C₄ e₃))
           (sqrt μ)))))

(define Carlson-elliptic₁ Rf)
(define Carlson-elliptic-1 Rf)

(define (Carlson-elliptic₁-simple x y z)
  (define ε (sqrt machine-ε))
  (let Rf₁ ([x x]
            [y y]
            [z z])
    (let ([μ (/ (+ x y z) 3.0)])
      (if (< (max (abs (/ (- x μ) μ))
                  (abs (/ (- y μ) μ))
                  (abs (/ (- z μ) μ)))
             ε)
          (/ 1.0 (sqrt μ))
          (let ([λ (+ (sqrt (* x y))
                      (sqrt (* x z))
                      (sqrt (* y z)))])
            (Rf₁ (/ (+ x λ) 4.0)
                 (/ (+ y λ) 4.0)
                 (/ (+ z λ) 4.0)))))))

(define Carlson-elliptic-1-simple Carlson-elliptic₁-simple)

(define (Rd x y z)
  (define ε (sqrt machine-ε))
  (define C₁ (/ -3. 14.))
  (define C₂ (/ 1. 6.))
  (define C₃ (/ -9. 22.))
  (define C₄ (/ 3. 26.))
  (define C₅ (* .25 C₃))
  (define C₆ (* -1.5 C₄))
  (let loop ([x x]
             [y y]
             [z z]
             [sum 0.]
             [fac 1.])
    (let* ([√x (sqrt x)]
           [√y (sqrt y)]
           [√z (sqrt z)]
           [λₐ (+ √x (+ √y √z) (* √y √z))]
           [sump (+ sum (/ fac (* √z (+ z λₐ))))]
           [facp (* .25 fac)]
           [xp (* .25 (+ x λₐ))]
           [yp (* 1/4 (+ y λₐ))]
           [zp (* .25 (+ z λₐ))]
           [μ (* .2 (+ xp yp (* 3. zp)))]
           [Δx (/ (- μ xp) μ)]
           [Δy (/ (- μ yp) μ)]
           [Δz (/ (- μ zp) μ)])
      (if (> (max (abs Δx) (abs Δy) (abs Δz)) ε)
          (loop xp yp zp sump facp)
          (let* ([ea (* Δx Δy)]
                 [eb (* Δz Δz)]
                 [ec (- ea eb)]
                 [ed (- ea (* 6. eb))]
                 [ee (+ ed ec ec)])
            (+ (* 3 sump)
               (/ (* facp
                     (+ (+ 1.
                           (* ed
                              (+ C₁ (* C₅ ed) (* C₆ Δz ee))))
                        (* Δz (+ (* C₂ ee) (* Δz (+ (* C₃ ec) (* Δz C₄ ea)))))))
                  (* μ (sqrt μ)))))))))

(define Carlson-elliptic₂ Rd)
(define Carlson-elliptic-2 Rd)

(define (elliptic-integral-F φ k)
  (define sinφ (sin φ))
  (* sinφ
     (Carlson-elliptic₁ (square (cos φ))
                        (* (- 1. (* sinφ k))
                           (+ 1. (* sinφ k)))
                        1.)))

(define (complete-elliptic-integral-K k)
  (elliptic-integral-F π/2 k))

(define (elliptic-integral-E φ k)
  (define sinφ (sin φ))
  (define cos²φ (square (cos φ)))
  (define q (* (- 1. (* sinφ k))
               (+ 1. (* sinφ k))))
  (* sinφ
     (- (Carlson-elliptic₁ cos²φ q 1.)
        (* (square (* sinφ k))
           (/ (Carlson-elliptic₂ cos²φ q 1.) 3.)))))

(define (complete-elliptic-integral-E k)
  (elliptic-integral-E π/2 k))

;;; older definition of the complete elliptic integrals
;;; probably from A&Stegun

(define (elliptic-integrals k continue)
  (if (= k 1)
      (continue +inf.0 1.)
      (let loop ([a 1.0]
                 [b (sqrt (- 1.0 (square k)))]
                 [c k]
                 [d 0.0]
                 [powers-2 1.0])
        (if (< (abs c) machine-ε)
            (let ([first-elliptic-integral (/ π/2 a)])
              (continue first-elliptic-integral
                        (* first-elliptic-integral
                           (- 1.0 (/ d 2.0)))))
            (loop (/ (+ a b) 2.0)
                  (sqrt (* a b))
                  (/ (- a b) 2.0)
                  (+ d (* (square c)
                          powers-2))
                  (( powers-2 2.0)))))))

;; K
(define (first-elliptic-integral k)
  (elliptic-integrals k (λ (K E) K)))

;; E
(define (second-elliptic-integral k)
  (elliptic-integrals k (λ (K E) E)))

(define (first-elliptic-integral&derivative k cont)
  (if (= k 0.0)
      (cons π/2 0.0)
      (elliptic-integrals
       k
       (λ (Kk Ek)
         (cont Kk
               (/ (- (/ Ek (- 1 (square k)))
                     Kk)
                  k))))))

;; TODO: refactor this, but write tests first to make sure we didnt
;; break anything.
(define Jacobi-elliptic-functions
  (let ((eps (sqrt machine-ε)))
    (lambda (uu k cont)
      ;; (cont sn cn dn)
      (let ((emc (- 1. (square k)))
	    (u uu)
	    (d 1.0))
	(if (= emc 0.) 
	    (let ((cn (/ 1. (cosh u))))
	      (cont (tanh u) cn cn))
	    (let ((bo (< emc 0.)))
	      (when bo 
                (begin
                  (set! d (- 1. emc))
                  (set! emc (- (/ emc d)))
                  (set! d (sqrt d))
                  (set! u (* u d))))
	      (let ((dn 1.))
		(let loop ((a 1.) (emc emc) (i 1) (em '()) (en '()))
		  (let ((emc (sqrt emc)))
		    (let ((c (* 0.5 (+ a emc))))
		      (if (and (> (abs (- a emc)) (* eps a))
			       (fx< i 13))
			  (loop c (* a emc) (fx+ i 1) (cons a em) (cons emc en))
			  ;; label 1
			  (let* ((u (* c u))
				 (sn (sin u))
				 (cn (cos u)))
			    (when (not (= sn 0.)) 
                              (let* ((a (/ cn sn))
                                     (c (* a c)))
                                (let loop2 ((em em) (en en))
                                  (if (and (not (null? em)) (not (null? en)))
                                      (let ((b (car em)))
                                        (set! a (* c a))
                                        (set! c (* dn c))
                                        (set! dn (/ (+ (car en) a) (+ a b)))
                                        (set! a (/ c b))
                                        (loop2 (cdr em) (cdr en)))
                                      (let ((a (/ 1. (sqrt (+ 1. (square c))))))
                                        (if (< sn 0.)
                                            (set! sn (- a))
                                            (set! sn a))
                                        (set! cn (* c sn)))))))
			    (if bo 
				(cont (/ sn d) a cn)
				(cont sn cn dn))))))))))))))
