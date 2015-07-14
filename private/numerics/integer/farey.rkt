#lang racket/base

;; Farey Trees (aka Stern-Brocot Trees)
;; http://mathworld.wolfram.com/FareySequence.html

;; In a Farey tree each level is constructed by
;; adjoining the mediants of the elements of the previous level
;; into the previous level.
;;
;; Because the mediants are always between the given fractions
;; the levels are ordered if the first one is ordered.

(provide farey)

;; -----------------------------------------------------------------------------

(require
 racket/match
 mechanics
 (only-in racket/math pi sqr)
 (only-in racket/flonum flmax)
 (only-in math/base phi.0)
 (only-in racket/set set
                     for/set
                     set->list
                     set-union)
 )

;; =============================================================================

;; bg: The "rational" with numerator 1 and denominator 0.
(define INF 'inf)

(define (numerator/inf n)
  (or (and (eq? n INF) 1)
      (numerator n)))

(define (denominator/inf n)
  (or (and (eq? n INF) 0)
      (denominator n)))

(define (mediant m1/n1 m2/n2)
  (/ (+ (numerator/inf m1/n1) (numerator/inf m2/n2))
     (+ (denominator/inf m1/n1) (denominator/inf m2/n2))))

(define ((farey lo hi) n)
  (define (nlp n)
    (if (= n 1)
        (list lo hi)
        (llp (nlp (- n 1)) '())))
  (define (llp in out)
    (match in
      [(cons a '())
       (reverse (cons a out))]
      [(cons a (cons b rest))
       (llp (cons b rest)
            (cons (mediant a b)
                  (cons a out)))]))
  (nlp n))

(define (farey? sequence)
  (define (lp n s)
    (cond
     [(< n 3)
      #t]
     [(= (mediant (car s) (caddr s)) (cadr s))
      (lp (- n 1) (cdr s))]
     [else
      #f]))
  (lp (length sequence) sequence))

;; The Farey sequence of level n is the ordered list of fractions
;; between 0 and 1 that have no denominators greater than n.
(define (farey-sequence-naive n)
  (for/list ([p/q ((farey 0 1) n)]
             #:when (not (> (denominator p/q) n)))
    p/q))

;; The full Farey sequence is useful for understanding the
;; convergents of continued fractions.
;; For any irrational the convergents are the closest numbers
;; on the full Farey sequences.
(define (full-farey-sequence n)
  (for/list ([p/q ((farey 0/1 INF) n)]
             #:when (not (> denominator p/q) n))
    p/q))

;; In the limit as n->infinity the length of the nth Farey sequence is
(define (farey-length n)
  (* (/ 3 (sqr pi)) (sqr n)))

;; Of course, the computation of the nth Farey sequence by filtering
;; the nth level of the Farey tree is horribly inefficient.  There
;; are better ways of computing the nth Farey sequence:
(define (farey-sequence n)
  (define m (add1 n))
  (define (all-fractions-with-denom q)
    (for/set ([p (in-range 1 m)]
              #:when (not (> p q)))
      (/ p q)))
  (define farey-fractions
    (for/fold ([s (set)])
              ([q (in-range 0 m)])
      (set-union s (all-fractions-with-denom q))))
  ;; Prepend 0/0 to result; it's a member of any farey-sequence
  (cons 0 (sort (set->list farey-fractions) <)))

(define (lamothe-simplicity m/n)
  (/ 1 (* (numerator/inf m/n) (denominator/inf m/n))))

(define (farey-encode m/n)
  (define (lp m n)
    (cond
     [(= m n)
      '()]
     [(< m n)
      (cons 'L (lp m (- n m)))]
     [else
      (cons 'R (lp (- m n) n))]))
  (lp (numerator/inf m/n) (denominator/inf m/n)))

(define (farey-encode-real x maxlevel)
  (cond
   [(or (= x 1) (= maxlevel 0))
    '()]
   [(< x 1)
    (cons 'L
          (farey-encode-real (/ x (- 1 x))
                             (- maxlevel 1)))]
   [else;(> x 1)
    (cons 'R
          (farey-encode-real (- x 1)
                             (- maxlevel 1)))]))

(define (farey-decode L/R-list)
  (let lp ([lst L/R-list]
           [low 0/1]
           [hi  INF])
    (let ([x (mediant low hi)])
      (cond
       [(null? lst)
        x]
       [(eq? (car lst) 'L)
        (lp (cdr lst) low x)]
       [else
        (lp (cdr lst) x hi)]))))


;; =============================================================================

(module+ test
  (require rackunit
           (only-in racket/list drop-right))

  ;; -- farey

  (check-equal? ((farey 0 1) 1)
                (list 0 1))

  (check-equal? ((farey 0 1) 2)
                (list 0 1/2 1))

  (check-equal? ((farey 0 1) 3)
                (list 0 1/3 1/2 2/3 1))

  (check-equal? ((farey 0 1) 4)
                (list 0 1/4 1/3 2/5 1/2 3/5 2/3 3/4 1))

  (check-equal? ((farey 0 1) 5)
                (list 0 1/5 1/4 2/7 1/3 3/8 2/5 3/7
                      1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 1))

  ;; --- farey?

  (check-pred farey? ((farey 0 1) 15))

  ;; -- farey-sequence-naive

  (check-equal? (farey-sequence-naive 5)
                (list 0 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1))

  (check-equal? (farey-sequence-naive 6)
                (list 0 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1))

  (check-equal? (farey-sequence-naive 7)
                (list 0 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2
                      4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1))

  (check-pred farey? (farey-sequence-naive 7))

  (check-pred farey? (farey-sequence-naive 15))

  ;; -- farey-length

  (check-equal? (length (farey-sequence-naive 20))
                129)

  (check-equal? (farey-length 20)
                121.58542037080534)

  ;; -- farey-sequence

  (check-equal? (farey-sequence 5)
                (list 0 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1))

  (check-equal? (farey-sequence 6)
                (list 0 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1))

  (check-equal? (farey-sequence 7)
                (list 0 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2
                      4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1))

  (check-pred farey? (farey-sequence 7))

  (check-pred farey? (farey-sequence 15))

  ;; -- lamothe-simplicity
  ;; Theorem: The sums of the Lamothe-simplicities of the interior
  ;; members of the nth Farey tree level from 0 to infinity is n-1
  (define (interior xs)
    (cdr (drop-right xs 1)))

  (define (lamothe-simplicity-sum f-seq)
    (for/sum ([n (in-list (interior f-seq))])
      (lamothe-simplicity n)))

  (define-syntax-rule (check-lamothe [farey-seq expected] ...)
    (begin
      (check-equal? (lamothe-simplicity-sum farey-seq) expected)
      ...))

  (check-lamothe
   [((farey 0 INF) 5) 4]
   [((farey 0 INF) 6) 5]
   [((farey 0 INF) 10) 9])

  ;; -- farey-encode

  (check-equal? (farey-encode 2/7)
                '(L L L R))

  (check-equal? (farey-encode 5/8)
                '(L R L R))

  (check-equal? (farey-encode-real 5/8 10)
                '(L R L R))

  (check-equal? (farey-encode-real pi 20)
                '(R R R L L L L L L L R R R R R R R R R R))

  (check-equal? (farey-encode-real phi.0 20)
                '(R L R L R L R L R L R L R L R L R L R L))

  ;; -- farey-decode

  (check-equal? (farey-decode (farey-encode 3/7))
                3/7)

  (check-equal? (farey-decode (farey-encode 3/8))
                3/8)

  ;; Farey-encoding is not very efficient.
  (check-equal? (farey-decode (farey-encode-real pi 25))
                355/113)

)
