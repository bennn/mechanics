#lang racket/base

;; Continued Fractions stream approximations of real numbers.
;; http://en.wikipedia.org/wiki/Continued_fraction

(provide
 ;; (-> Exact-Rational (Streamof Integer))
 ;; Create the continued fraction representation of an exact number.
 continued-fraction
 ;; (-> (Streamof Integer) (Streamof Exact-Rational))
 ;; Return a stream of approximation to a continued fraction.
 ;; The nth element of the output uses the first n terms of the
 ;; continued fraction.
 convergents
)

;; -----------------------------------------------------------------------------

(require
 racket/stream
 (only-in racket/math
          exact-floor
          pi))

;; =============================================================================
;; API Functions

(define (continued-fraction x)
  ;; (-> Number (Streamof Integer))
  (let ([a (exact-floor x)])
    (stream-cons a (continued-fraction (/ 1 (- x a))))))

(define (convergents cfrac)
  ;; (-> (Streamof Integer) (Streamof Exact-Rational))
  (define (gen A1 A2 B1 B2 cfrac)
    ;; (-> Integer Integer Integer Integer (Streamof Exact-Rational) (Streamof Exact-Rational))
    (let ((b (stream-first cfrac)))
      (let ([An (+ (* b A1) A2)]
            [Bn (+ (* b B1) B2)])
        (stream-cons (/ An Bn)
                     (gen An A1 Bn B1 (stream-rest cfrac))))))
  (let ((b (stream-first cfrac)))
    (stream-cons b
                 (gen b 1 1 0 (stream-rest cfrac)))))

;; =============================================================================
;; Tests

(module+ test
  (require rackunit)

  (define stream-ones (stream-cons 1 stream-ones))

  (define (take st n)
    ;; (-> (Streamof Any) Natural (Listof Any))
    (for/list ([_ (in-range n)]
               [s (in-stream st)])
              s))

  (define pi-convergents (convergents (continued-fraction pi)))

  ;; --- tests

  (check-equal? (take stream-ones 5) (list 1 1 1 1 1))

  (check-equal? (take (continued-fraction pi) 10)
                (list 3 7 15 1 292 1 1 1 2 1))

  (check-equal? (take (continued-fraction (exp 1)) 30)
                (list 2 1 2 1 1 4 1 1 6 1 1 8 1 1 10 1
                      1 12 1 1 11 3 2 1 3 1 73 6 1 1))

  (check-equal? (take pi-convergents 10)
                (list 3 22/7 333/106 355/113 103993/33102 104348/33215
                      208341/66317 312689/99532 833719/265381 1146408/364913))

  (check-equal? (take (stream-map (lambda (x) (/ (- x pi) pi))
                                  pi-convergents) 10)
                (list -.04507034144862795 4.024994347707008e-4
                      -2.648963016704766e-5 8.49136787674061e-8
                      -1.8394829882935047e-10 1.0556048950798648e-10
                      -3.894723497538627e-11 9.276617820935258e-12
                      -2.7741504721654008e-12 5.127054146519189e-13))

  (check-equal? (take (convergents stream-ones) 10)
                (list 1 2 3/2 5/3 8/5 13/8 21/13 34/21 55/34 89/55))

  (check-equal? (take (stream-map
                       (lambda (x)
                         (let ((gm (/ (+ 1 (sqrt 5)) 2)))
                           (/ (- x gm) gm)))
                       (convergents stream-ones)) 10)
                (list -0.38196601125010515 0.23606797749978967
                      -0.07294901687515776 0.030056647916491423
                      -0.01114561800016822 0.004305231718579094
                      -0.0016374027886314115 0.0006264579760202099
                      -0.0002391358457583512 9.136361346616536e-05))

  (check-equal? (take (stream-map
                       (lambda (x)
                         (let ([gm (/ (+ 1 (sqrt 5)) 2)])
                           (/ (- x gm) gm)))
                       (convergents stream-ones)) 10)
                (list -.38196601125010515 .23606797749978967
                      -.07294901687515776 3.0056647916491423e-2
                      -.01114561800016822 4.305231718579094e-3
                      -1.6374027886314115e-3 6.264579760202099e-4
                      -2.391358457583512e-4 9.136361346616536e-5))

  (check-equal? (take (continued-fraction (sqrt 2)) 20)
                (list 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2))

  (check-equal? (take (convergents (continued-fraction (sqrt 2))) 10)
                (list 1 3/2 7/5 17/12 41/29 99/70 239/169
                      577/408 1393/985 3363/2378))

  (check-equal? (take  (stream-map
                        (lambda (x)
                          (let ((r2 (sqrt 2)))
                            (/ (- x r2) r2)))
                        (convergents
                         (continued-fraction (sqrt 2))))
                       10)
                (list -.29289321881345254 .06066017177982121
                      -1.0050506338833596e-2 .00173460668094231
                      -2.973093569501634e-4 5.101910668863161e-5
                      -8.753233225833026e-6 1.5018250929351827e-6
                      -2.576722227077141e-7 4.420957066726649e-8))

  ;; check that the error is less than 1/d^2
  (check-equal? (take (let ([number (sqrt 2)])
                        (stream-map (lambda (x)
                                      (let ([error (abs (- x number))])
                                        (- error
                                           (/ 1 (expt (denominator x) 2)))))
                                    (convergents (continued-fraction number))))
                      10)
                (list -0.5857864376269049 -0.16421356237309515
                      -0.025786437626904767 -0.004491340150872849
                      -0.000768601717273402 -0.00013192972003383852
                      -2.263383852198971e-05 -3.8834034679962394e-06
                      -6.662852057623048e-07 -1.143165126209822e-07))

  (check-equal? (take  (let ((number (/ (+ 1 (sqrt 5)) 2)))
                         (stream-map
                          (lambda (c)
                            (let ((error (abs (- c number))))
                              (- error (/ 1 (expt (denominator c) 2)))))
                          (convergents
                           (continued-fraction number)))) 10)
                (list -0.3819660112501051 -0.6180339887498949
                      -0.1319660112501051 -0.06247843319433927
                      -0.021966011250105187 -0.008658988749894903
                      -0.0032677863980341256 -0.0012539433984209585
                      -0.00047812197674872224 -0.0001827490804734311))

  (check-equal? (take (let ((number pi))
                        (stream-map
                         (lambda (c)
                           (let ((error (abs (- c number))))
                             (- error (/ 1 (expt (denominator c) 2)))))
                         (convergents
                          (continued-fraction number)))) 20)
                (list -.8584073464102069 -1.9143673997956443e-2
                      -5.780016472316501e-6 -7.804790414797462e-5
                      -3.347326313231945e-10 -5.747961301800781e-10
                      -1.0502260270843243e-10 -7.179925756609443e-11
                      -5.4838338802403175e-12 -5.89896667061906e-12
                      -1.3644086097276364e-13 -3.1384653391315194e-13
                      -1.0925052623624082e-15 -1.6328867414587005e-16
                      -1.4760984937032654e-17 -2.718809562442266e-20
                      -2.499649407750906e-20 -6.51444233883359e-21
                      -1.154885932677522e-22 -6.760478215475832e-24))
)
