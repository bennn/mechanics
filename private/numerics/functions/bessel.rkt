#lang mechanics

;; This module defines the bessel functions of integer order
(provide
 (contract-out
  [bessj₀ (-> number? number?)]
  [bessj₁ (-> number? number?)]
  [bessj  (-> integer? number? number?)]
  [bessy₀ (-> number? number?)]
  [bessy₁ (-> number? number?)]
  [bessy  (-> integer? positive? number?)]
  [bessh₀ (-> number? complex?)]
  [bessh₁ (-> number? complex?)]
  [bessh  (-> integer? number? complex?)]))

(require
 (only-in math/number-theory factorial))

;; Utilities for special functions

(define (round-to-even x)
  (let ([xn (inexact->exact (round x))])
    (if (odd? xn) (+ xn 1) xn)))

;; computes ∑ cᵢ xⁱ
(define (poly-by-coeffs->value x . coeffs)
  (let lp ([coeffs coeffs])
    (if (null? (cdr coeffs))
        (car coeffs)
        (+ (car coeffs)
           (* x (lp (cdr coeffs)))))))

;;; From John F. Hart, et.al., Computer Approximations, QA297.C64 1968
;;;           These are all good to at least 15 digits.
(define (bessj₀ x)
  (let ([ax (magnitude x)])
    (if (< ax 8.0)
        (let ([x² (* x x)])
          (/ (poly-by-coeffs->value
              x²
              +.1859623176218978035283999449e+18
              -.4414582939181598183458448718e+17
              +.2334489171877869744571586698e+16
              -.4776555944267358775465713161e+14
              +.4621722250317180263694186830e+12
              -.2271490439553603267422190396e+10
              +.5513584564770752154116759317e+7
              -.5292617130384557364907747176e+4)
             (poly-by-coeffs->value
              x²
              +.1859623176218977331294574009e+18
              +.2344750013658996756881142774e+16
              +.1501546244976975197238575580e+14
              +.6439867453513325627846468877e+11
              +.2042514835213435736159365899e+9
              +.4940307949181397241772754336e+6
              +.8847203675617550401186701293e+3
              +.1e+1)))
        (let* ([z (/ 8.0 ax)]
               [z² (* z z)]
               [ax-π/4 (- ax π/4)]
               [p₀ (/ (poly-by-coeffs->value
                       z²
                       +.8554822541506661710252074e+4
                       +.8894437532960619440762804e+4
                       +.2204501043965180428995069e+4
                       +.1286775857487141932988510e+3
                       +.9004793474802880316384000e+0)
                      (poly-by-coeffs->value
                       z²
                       +.8554822541506662842462151e+4
                       +.8903836141709595355210343e+4
                       +.2214048851914710419825683e+4
                       +.1308849004999238828351090e+3
                       +.1e+1))]
               [q₀ (/ (poly-by-coeffs->value
                       z²
                       -.37510534954957111594836e+2
                       -.46093826814625174976377e+2
                       -.13990976865960680088016e+2
                       -.10497327982345548331260e+1
                       -.93525953294031893049e-2)
                      (poly-by-coeffs->value
                       z²
                       +.2400674237117267479318819e+4
                       +.2971983745208491990065486e+4
                       +.921566975526530895082307e+3
                       +.74428389741411178824152e+2
                       +.1e1))])
          (* (sqrt (/ 2/π ax))
             (- (* (cos ax-π/4) p₀)
                (* z (sin ax-π/4) q₀)))))))

(define (bessj₁ x)
  (let ([ax (magnitude x)])
    (if (< ax 8.0)
        (let ([x² (* x x)])
          (/ (* x
                (poly-by-coeffs->value
                 x²
                 +.695364226329838502166085207e+8
                 -.8356785487348914291918495672e+7
                 +.3209027468853947029888682298e+6
                 -.58787877666568200462723094e+4
                 +.6121876997356943874446879769e+2
                 -.3983107983952332023421699105e+0
                 +.1705769264349617107854016566e-2
                 -.4910599276555129440130592573e-5
                 +.9382193365140744507653268479e-8
                 -.1107352224453730633782671362e-10
                 +.63194310317443161294700346e-14))
             (poly-by-coeffs->value
              x²
              +.139072845265967685120764336e+9
              +.6705346835482299302199750802e+6
              +.1284593453966301898121332163e+4
              +.1e+1)))
        (let* ([z (/ 8.0 ax)]
               [z² (* z z)]
               [ax-3π/4 (- ax 3π/4)]
               [p₁ (/ (poly-by-coeffs->value
                       z²
                       +.1290918471896188077350689e+5
                       +.1309042051103506486292571e+5
                       +.313275295635506951011069e+4
                       +.17431379748379024599685e+3
                       +.122850537643590432633e+1)
                      (poly-by-coeffs->value
                       z²
                       +.1290918471896187879332737e+5
                       +.1306678308784402036110575e+5
                       +.310928141677002883350924e+4
                       +.16904721775008609992033e+3
                       +.1e+1))]
               [q₁ (/ (poly-by-coeffs->value
                       z²
                       ;This line or the next is in error
                       ;+.14465282874995208675225e+3   
                       +.14465282874995208765225e+3
                       +.1744291689092425885102e+3
                       +.5173653281836591636536e+2
                       +.379944537969806734901e+1
                       +.36363466476034710809e-1)
                      (poly-by-coeffs->value
                       z²
                       +.308592701333231723110639e+4
                       +.373434010601630179517765e+4
                       +.11191098527047487025919e+4
                       +.8522392064341340397334e+2
                       +.1e+1))]
               [ans (* (sqrt (/ 2/π ax))
                       (- (* (cos ax-3π/4) p₁)
                          (* z (sin ax-3π/4) q₁)))])
          (if (< x 0.0) (- ans) ans)))))

(define (bessj n x)
  (define acc 40)
  (define bigno 1.0e10)
  (define bigni 1.0e-10)
  (cond
    [(= n 0) (bessj₀ x)]
    [(= n 1) (bessj₁ x)]
    [(= x 0.0) 0.0]
    [(< x 0.0) (if (even? n)
                   (bessj n (- x))
                   (- (bessj n (- x))))]
    [(< n 0) (if (even? n)
                   (bessj (- n) x)
                   (- (bessj (- n) x)))]
    [else
     (define ax (magnitude x))
     (define 2/ax (/ 2.0 ax))
     (if (> ax n)
         (let-values ([(bjm bj)
                       (for/fold ([bjm (bessj₀ ax)]
                                  [bj (bessj₁ ax)])
                                 ([j (in-range 1 n)])
                         (values bj (- (* j 2/ax bj) bjm)))])
           (if (and (< x 0) (odd? n)) (- bj) bj))
         (let ((m (round-to-even (+ n (sqrt (* acc n)))))
               (bj 1.0)
               (bjp 0.0)
               (ans 0.0)
               (sum 0.0))
           (let lp ((j m))
             (if (= j 0)
                 (let ((ans (/ ans (- (* 2.0 sum) bj))))
                   (if (and (< x 0.0) (odd? n)) (- ans) ans))
                 (let ((bjm (- (* j 2/ax bj) bjp)))
                   (set! bjp bj)
                   (set! bj bjm)
                   (when (> (magnitude bj) bigno)
                     (begin (set! bj (* bj bigni))
                            (set! bjp (* bjp bigni))
                            (set! ans (* ans bigni))
                            (set! sum (* sum bigni))))
                   (when (odd? j)
                     (set! sum (+ sum bj)))
                   (when (= j n)
                     (set! ans bjp))
                   (lp (- j 1)))))))]))

;; NOTE: below was the attempted refactor of the above. For some reason
;; this didn't work.
;;
;; TODO: attempt another refactor maybe using below as a starting point
;;
;; (let*-values ([(start) (round-to-even (+ n (sqrt (* acc n))))]
;;                       [(bj bjp ans sum)
;;                        (for/fold ([bj  1]
;;                                   [bjp 0]
;;                                   [ans 0]
;;                                   [sum 0])
;;                            ([j (in-range start -1 -1)])
;;                          (let* ([bjm (- (* j 2/ax bj) bj)])
;;                            (set! bjp bj)
;;                            (set! bj bjm)
;;                            (when (> (magnitude bj) bigno)
;;                              (set! bj (* bj bigni))
;;                              (set! bjp (* bjp bigni))
;;                              (set! ans (* ans bigni))
;;                              (set! sum (* sum bigni)))
;;                            (when (odd? j)
;;                              (set! sum (+ sum bjm)))
;;                            (when (fx= j n)
;;                              (set! ans bj))
;;                            (values bj bjp ans sum)))]
;;                       [(out) (/ ans (* 2 sum) bj)])
;;           (displayln (format "x: ~a, start: ~a, out: ~a"
;;                              x start out))
;;           (if (and (< x 0) (odd? n)) (- out) out))

(define (bessy₀ x)
  (define ax (magnitude x))
  (if (< ax 8.0)
      (let* ([x² (* x x)]
             [r₀ (poly-by-coeffs->value
                  x²
                  -.122848349966864707119444888e+8  ;p00
                  +.2950673961329634647867906439e+8 ;p01
                  -.2540763578168434015208700066e+7 ;p02
                  +.7768806299511773765193176993e+5 ;p03
                  -.1193299661108745921129349868e+4 ;p04
                  +.10753556131901778914962135e+2   ;p05
                  -.6186687126256085875960782886e-1 ;p06
                  +.2379830688791742855598085169e-3 ;p07
                  -.6227852796374134180786140767e-6 ;p08
                  +.1091804574277522610752537393e-8 ;p09
                  -.119120749069566983004259626e-11 ;p10
                  +.6321369945552678896098605e-15   ;p11
                  )]
             [s₀ (poly-by-coeffs->value
                  x²
                  +.1664514914558198835968659363e+9 ;q00
                  +.75939734950276133884153062e+6   ;q01
                  +.137072109013171838997378226e+4  ;q02
                  +1.0		                    ;q03
                  )])
        (+ (/ r₀ s₀)
           (* 2/π (bessj₀ x) (log x))))
      (let* ([z (/ 8.0 ax)]
             [z² (* z z)]
             [ax-π/4 (- ax π/4)]
             [p₀ (/ (poly-by-coeffs->value
                     z²
                     +.8554822541506661710252074e+4
		     +.8894437532960619440762804e+4
		     +.2204501043965180428995069e+4
		     +.1286775857487141932988510e+3
		     +.9004793474802880316384000e+0)
                    (poly-by-coeffs->value
                     z²
                     +.8554822541506662842462151e+4
		     +.8903836141709595355210343e+4
		     +.2214048851914710419825683e+4
		     +.1308849004999238828351090e+3
		     +.1e+1))]
             [q₀ (/ (poly-by-coeffs->value
                     z²
                     -.37510534954957111594836e+2
		     -.46093826814625174976377e+2
		     -.13990976865960680088016e+2
		     -.10497327982345548331260e+1
		     -.93525953294031893049e-2)
                    (poly-by-coeffs->value
                     z²
                     +.2400674237117267479318819e+4
		     +.2971983745208491990065486e+4
		     +.921566975526530895082307e+3
		     +.74428389741411178824152e+2
		     +.1e1))])
        (* (sqrt (/ 2/π ax))
           (+ (* (sin ax-π/4) p₀)
              (* z (cos ax-π/4) q₀))))))

(define (bessy₁ x)
  (let [(ax (magnitude x))]
    (if (< ax 8)
        (let* [(x² (* x x))
              (r₀
               (poly-by-coeffs->value
                x²
                -0.4900604943e13
                +0.1275274390e13
                -0.5153438139e11
                +0.7349264551e9
                -0.4237922726e7
                +0.8511937935e4))
              (s₀
               (poly-by-coeffs->value
                x²
                +0.2499580570e14
                +0.4244419664e12
                +0.3733650367e10
                +0.2245904002e8
                +0.1020426050e6
                +0.3549632885e3
                +1.0))]
          (+ (/ (* x r₀)
                s₀)
             (* 2/π
                (- (* (bessj₁ x)
                      (log x))
                   (/ 1 x)))))
        (let* [(z (/ 8 x))
               (z² (* z z))
               (ax-3π/4 (- ax 3π/4))
               (p₀
                (poly-by-coeffs->value
                 z²
                 +1.0
                 +0.183105e-2
                 -0.3516396496e-4
                 +0.2457520174e-5
                 -0.240337019e-6))
               (q₀
                (poly-by-coeffs->value
                 z²
                 +0.04687499995
                 -0.2002690873e-3
                 +0.8449199096e-5
                 -0.88228987e-6
                 +0.105787412e-6))]
          (* (sqrt (/ 2/π ax))
             (+ (* (sin ax-3π/4)
                   p₀)
                (* z
                   (cos ax-3π/4)
                   q₀)))))))

(define (bessy n x)
  (cond
    [(= n 0) (bessy₀ x)]
    [(= n 1) (bessy₁ x)]
    [(< n 0)
     (if (even? n)
         (bessy (- n) x)
         (- (bessy (- n) x)))]
    [else
     (let lp ((i 1) (yn (bessy₁ x)) (yn-1 (bessy₀ x)))
       (if (= i n)
           yn
           (lp (+ i 1)
               (- (/ (* 2 i yn) x) yn-1)
               yn)))]))


;; Hankel functions H¹
(define (bessh n x)
  (+ (bessj n x)
     (* +i (bessy n x))))

(define (bessh₀ x) (bessh 0 x))

(define (bessh₁ x) (bessh 1 x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                                  TESTING                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require
   (only-in mechanics 2π)
   (only-in racket/math exact-round)
   (only-in rackunit test-suite test-case check-=)
   (only-in rackunit/text-ui run-tests))

  ;; Asymptotic formulae, for testing
  (define (bessj:0<x<<n n x)
    ;; HACK: sometimes n gets converted to a floating version of the
    ;; integer such as 1000.0
    (/ (expt (/ x 2) n) (factorial (exact-round n))))

  (define (bessj:n<<x n x)
    (* (sqrt (/ 2 π x))
       (cos (- x (* π/2 n) π/4))))

  (define (bessh:n<<x n x)
    (* (sqrt (/ 2 π x))
       (exp (* +i (- x (* π/2 n) π/4)))))

  ;; Consistency check based on Wronskian:

  ;; J_{n+1}(x) Y_n(x) - J_n(x) Y_{n+1}(x) = 2/(pi x)
  (define (bessel-wronskian n x)
    (- (* (bessj (add1 n) x) (bessy n x))
            (* (bessj n x) (bessy (add1 n) x))))

  (run-tests
   (test-suite
    "Test suite for Bessel functions."
    (test-case "Wronskian Check"
      (for* ([n (in-range -100 100)]
             ;; Beware the singularity of Y at 0
             [x (in-range .1 2π)])
         (check-= (bessel-wronskian n x)
                  (/ 2 (* π x))
                  1e-9
                  (format
                   "Wronskian check failure with n = ~a and x = ~a" n x))))
    
    (test-case "Bessel J Small x large n asymptotic"
      (for* ([n (in-range 1000 10000 100)]
             [x (in-range .0001 .001 .0001)])
        (check-= (bessj n x)
                 (bessj:0<x<<n n x)
                 1e-9
                 (format
                  "Asymptotic formula failed for 0 < x = ~a << n = ~a" n x))))

    (test-case "Bessel J Small n large n asymptotic"
      (for* ([n (in-range 1 10)]
             [x (in-range 1e3 1e4 100)])
        (check-= (bessj n x)
                 (bessj:n<<x n x)
                 1e-3
                 (format
                  "Asymptotic formula failed for 0 < x = ~a << n = ~a" n x))))
    
    (test-case "Bessel H Small n large x"
      (for* ([n (in-range 1 10)]
             [x (in-range 1e3 1e4 100)])
        (check-= (bessh n x)
                 (bessh:n<<x n x)
                 1e-2
                 (format
                  "Asymptotic formula failed for 0 < x = ~a << n = ~a" n x)))))))
