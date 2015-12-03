#lang mechanics

;;;; Bessel Functions:

;;; Jn+1(x) = 2n/x Jn(x) - Jn-1(x)   ------   unstable!

;;;   But Jn-1(x) = 2n/x Jn(x) - Jn+1(x) is stable, so use Miller trick.

;;;  (bessjs n x) returns a list
;;;    ( J0(x) ... Jn(x) ) 
;;;   that is good to machine precision for x < 2 and large n.

(provide
 (contract-out
  [bessjs (-> exact-nonnegative-integer? positive? (listof number?))]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                                  TESTING                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit
           rackunit/text-ui
           math/matrix
           (only-in racket/vector vector-map)
           (only-in racket/format ~a)
           (only-in racket/list range))

  (run-tests
   (test-suite
    "Test suite for bessel functions of integer order"
    (test-case
        "Output matches wolfram alpha."
      (let* ([max-bessel-order 4]
             [input-values (vector π/2 π 3π/2 2π)]
             [wolfram-alpha-output
              (matrix
               [[0.4720012157682347674476683878725009623642404480476665
                 0.5668240889058739377112449634671602835403174155414899
                 0.2497016291352035437004568140598349286692444512504045
                 0.0690358882935960517681315419235345127865894136174168
                 0.0139960398087738087561476475928853212701258364902632] 
                [-0.30424217764409386420203491281770492396965053478389
                 0.2846153431797527573453105996861314057098111818494657
                 0.4854339326315091097054957161831892378519525806986894
                 0.3334583362029895353902185817398867483022440044539132
                 0.1514245776313497107537095593154838070387599251684023] 
                [-0.26585724995832447634200508572810409283585670423879
                 -0.28165790875051959437304046973269013136918561636253
                 0.1463179207888012887168984214355549137790433534568295
                 0.4058564173183388935941409692546731240060510655130263
                 0.3704345192254447209477698577857675960813567380242590] 
                [0.2202769085399344622768816507208408344549146363474951
                 -0.21238253007636905220285865567108499622725602585368
                 -0.28788036751596899440312761555263523520769185918393
                 0.0291121960392572124902717611029454518524876804952073
                 0.3156804669394174890804788139717018390997948930552267]])]
             [actual-output
              (vector*->matrix
               (vector-map (λ (input-value)
                             (list->vector (bessjs max-bessel-order
                                                   input-value)))
                           input-values))
              ])
        (for* ([i (in-range (matrix-num-rows actual-output))]
               [j (in-range (matrix-num-cols actual-output))])
          (let ([actual (matrix-ref actual-output i j)]
                [expected (matrix-ref wolfram-alpha-output i j)])
            (check-= actual
                     expected
                     8e-14
                     (~a "Value of bessel J of order " j
                         " at input value " (vector-ref input-values i)
                         " does not match Wolfram Alpha. "
                         "Got: " actual
                         ". Expected: " expected)))))))))
