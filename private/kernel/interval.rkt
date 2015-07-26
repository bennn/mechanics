#lang racket/base

;; Utilities for working with subsets of the real line.
;; Alternatively, for representing data points that have a tolerance.

;; The `plot-lib` library's `common/math.rkt` gives a similar abstraction
;; for closed intervals

;; Missing from scmutils
;; - canonical form for lists of intervals
;;   (arranged from lowest lo bound upward, all intervals disjoint)

(provide
  ;; Constructor
  (rename-out
    [parse-interval      interval]
    [make-interval/check make-interval])
  ;; Accessors
  interval-lo
  interval-hi
  interval-lo-closed?
  interval-hi-closed?
  interval?
  ;; Utilities
  interval-empty?
;  #:contract (-> interval? boolean?)
;  #:doc "True if the argument is an empty interval. False otherwise."

  interval-lo-open?
;  #:contract (-> interval? boolean?)
;  #:doc "True if the lower bound of the interval is open."

  interval-hi-open?
;  #:contract (-> interval? boolean?)
;  #:doc "True if the upper bound of the interval is open."

  interval-disjoint?
;  #:contract (-> interval? interval? boolean?)
;  #:doc "True if the two intervals do not overlap"

  interval-intersect
;  #:contract (-> interval? interval? interval?)
;  #:doc "Return the (possibly-empty) interval obtained by intersecting the arguments"

  interval-union
;  #:contract (-> interval? interval? interval?)
;  #:doc "Take the union of two intervals.
;         If the arguments are disjoint, the result includes the space between them."

  interval-midpoint
;  #:contract (-> interval? number?)
;  #:doc "Calculate the point midway between the interval's lower and upper bounds"

  interval-contains?
;  #:contract (-> interval? number? boolean?)
;  #:doc "True if the interval contains the second argument."

  interval-translate
;  #:contract (-> interval? number? interval?)
;  #:doc "Create a new interval by adding a scalar to the bounds of the given interval"
)

;; -----------------------------------------------------------------------------

(require
  mechanics
  (for-syntax racket/base syntax/parse)
  (only-in racket/match match-define)
)

;; =============================================================================
;; Data Definition + Constructors

;; An interval is a lower and upper bound,
;; plus flags to indicate whether each bound is closed or open.
(struct interval (
  lo         ;; Real
  hi         ;; Real, must be >= lo
  lo-closed? ;; Boolean
  hi-closed? ;; Boolean
) #:transparent)

(define the-empty-interval 'the-empty-interval)

(define-syntax-rule (constructor-error msg arg* ...)
  (error 'interval (format msg arg* ...)))

;; Check invariants, then create an interval structure.
;; This instead be a dependent contract
(define (make-interval/check lo hi lo-closed? hi-closed?)
  (unless (number? lo)
    (constructor-error "Expected numeric lower bound, got '~a'" lo))
  (unless (number? hi)
    (constructor-error "Expected numeric upper bound, got '~a'" hi))
  (unless (boolean? lo-closed?)
    (constructor-error "Expected boolean for `lo-closed?`, got '~a'" lo-closed?))
  (unless (boolean? hi-closed?)
    (constructor-error "Expected boolean for `hi-closed?`, got '~a'" hi-closed?))
  (if (> lo hi)
    the-empty-interval
    (interval lo hi lo-closed? hi-closed?)))

;; We could possibly hack the #lang to distinguish square braces from parens.
;; (#lang mechanics)
(define-syntax (parse-interval stx)
  ;; Do NOT check `lo:number`, let `make-interval/check` test & raise the error
  (syntax-parse stx #:datum-literals (⟪ ⟫ ⟦ ⟧ ± %)
    [(_ ⟪ lo , hi ⟫)
     #'(make-interval/check lo hi #f #f)]
    [(_ ⟪ lo , hi ⟧)
     #'(make-interval/check lo hi #f #t)]
    [(_  ⟦ lo , hi ⟫)
     #'(make-interval/check lo hi #t #f)]
    [(_  ⟦ lo , hi ⟧)
     #'(make-interval/check lo hi #t #t)]
    ;; Number & tolerance
    [(_ center ± width)
     #'(make-interval/check (- center width) (+ center width) #t #t)]
    ;; Number & percentage
    [(ivl center ± pct %)
     #'(ivl center ± (* center (/ pct 100)))]
     ;#'(make-interval/check (- center width) (+ center width))]
    [_ (error 'interval "Could not parse syntax, expected an interval delimited by angle braces, got '~a'\n" stx)]))

;; =============================================================================

(define (interval-empty? ivl)
  (eq? ivl the-empty-interval))

(define (interval-lo-open? ivl)
  (not (interval-lo-closed? ivl)))

(define (interval-hi-open? ivl)
  (not (interval-hi-closed? ivl)))

(define (interval-disjoint? ivl1 ivl2)
  ;; Two intervals are disjoint if:
  (or
    ;; Either is empty
    (or (interval-empty? ivl1) (interval-empty? ivl2))
    ;; or the upper bound of one is less than the lower bound of another
    (or (< (interval-hi ivl1) (interval-lo ivl2))
        (< (interval-hi ivl2) (interval-lo ivl1)))
    ;; or the upper bound of one equals the lower bound of another,
    ;; and at least one of the two bounds is open
    (or (and (= (interval-hi ivl1) (interval-lo ivl2))
             (or (interval-hi-open? ivl1) (interval-lo-open? ivl2)))
        (and (= (interval-hi ivl2) (interval-lo ivl1))
             (or (interval-hi-open? ivl2) (interval-lo-open? ivl1))))))

(define (interval-intersect ivl1 ivl2)
  (cond
    [(or (interval-empty? ivl1) (interval-empty? ivl2))
     the-empty-interval]
    [(interval-disjoint? ivl1 ivl2)
     the-empty-interval]
    [else ;; Take the max lo and the min hi, be careful about open/closed
     (define-values (max-lo lo-closed?)
       (if (< (interval-lo ivl1) (interval-lo ivl2))
         (values (interval-lo ivl2) (interval-lo-closed? ivl2))
         ;; If the lo's are equal, return 'open' if either is open
         (values (interval-lo ivl1)
                 (if (= (interval-lo ivl1) (interval-lo ivl2))
                     (and (interval-lo-closed? ivl1) (interval-lo-closed? ivl2))
                     (interval-lo-closed? ivl2)))))
     (define-values (min-hi hi-closed?)
       (if (< (interval-hi ivl1) (interval-hi ivl2))
         (values (interval-hi ivl1) (interval-hi-closed? ivl1))
         ;; If the hi's are equal, return 'open' if either is open
         (values (interval-hi ivl2)
                 (if (= (interval-hi ivl1) (interval-hi ivl2))
                     (and (interval-hi-closed? ivl1) (interval-hi-closed? ivl2))
                     (interval-hi-closed? ivl2)))))
     (interval max-lo min-hi lo-closed? hi-closed?)]))

(define (interval-union ivl1 ivl2)
  (cond
    [(interval-empty? ivl1)
     ivl2]
    [(interval-empty? ivl2)
     ivl1]
    [else
     ;; Doesn't matter if the intervals are disjoint,
     ;; we absorb the difference into the new interval.
     (define-values (min-lo lo-closed?)
       (if (< (interval-lo ivl1) (interval-lo ivl2))
         (values (interval-lo ivl1) (interval-lo-closed? ivl1))
         ;; If the lo's are equal, return 'closed' if either is closed
         (values (interval-lo ivl2)
                 (if (= (interval-lo ivl1) (interval-lo ivl2))
                     (or (interval-lo-closed? ivl1) (interval-lo-closed? ivl2))
                     (interval-lo-closed? ivl2)))))
     (define-values (max-hi hi-closed?)
       (if (< (interval-hi ivl1) (interval-hi ivl2))
         (values (interval-hi ivl2) (interval-hi-closed? ivl2))
         ;; If the hi's are equal, return 'closed' if either is closed
         (values (interval-hi ivl1)
                 (if (= (interval-hi ivl1) (interval-hi ivl2))
                     (or (interval-hi-closed? ivl1) (interval-hi-closed? ivl2))
                     (interval-hi-closed? ivl1)))))
     (interval min-lo max-hi lo-closed? hi-closed?)]))

(define (interval-midpoint ivl)
  (and
    (not (interval-empty? ivl))
    (+ (interval-lo ivl)
       (/ (- (interval-hi ivl) (interval-lo ivl)) 2))))

(define (interval-contains? ivl x)
  (and
    (not (interval-empty? ivl))
    (or
      ;; `x` is within the bounds of the interval
      (and (< (interval-lo ivl) x)
           (< x (interval-hi ivl)))
      ;; `x` equals the closed lower bound
      (and (interval-lo-closed? ivl)
           (= (interval-lo ivl) x))
      ;; `x` equals the closed upper bound
      (and (interval-hi-closed? ivl)
           (= x (interval-hi ivl))))))

(define (interval-translate ivl x)
  (cond
    [(interval-empty? ivl)
      ivl]
    [else
     (interval (+ x (interval-lo ivl))
               (+ x (interval-hi ivl))
               (interval-lo-closed? ivl)
               (interval-hi-closed? ivl))]))

;; =============================================================================

(module+ test
  (require rackunit)

  ;; Constructors
  (check-equal? (parse-interval ⟪ 0 , 1 ⟫) (interval 0 1 #f #f))
  (check-equal? (parse-interval ⟪ 0 , 1 ⟧) (interval 0 1 #f #t))
  (check-equal? (parse-interval ⟦ 0 , 1 ⟫) (interval 0 1 #t #f))
  (check-equal? (parse-interval ⟦ 0 , 1 ⟧) (interval 0 1 #t #t))

  (check-equal? (parse-interval ⟪ 0 , 0 ⟫) (interval 0 0 #f #f))
  (check-equal? (parse-interval ⟪ 0 , 0 ⟧) (interval 0 0 #f #t))
  (check-equal? (parse-interval ⟦ 0 , 0 ⟫) (interval 0 0 #t #f))
  (check-equal? (parse-interval ⟦ 0 , 0 ⟧) (interval 0 0 #t #t))
  (check-equal? (parse-interval ⟪ 1 , 0 ⟫) the-empty-interval)

  (check-equal? (parse-interval 0.5 ± 0.5) (parse-interval ⟦ 0.0 , 1.0 ⟧))
  (check-equal? (parse-interval 0 ± 0.1) (parse-interval ⟦ -0.1 , 0.1 ⟧))
  (check-equal? (parse-interval 17 ± 1/3) (parse-interval ⟦ (- 17 1/3) , (+ 17 1/3) ⟧))
  (check-equal? (parse-interval 3 ± 10 %) (parse-interval ⟦ 27/10 , 33/10 ⟧))
  (check-equal? (parse-interval 91 ± 1 %) (parse-interval ⟦ 9009/100 , 9191/100 ⟧))


  ;; Utilities
  ;; -- interval-empty?
  (check-true (interval-empty? the-empty-interval))
  (check-false (interval-empty? (interval 0 1 #t #t)))
  (check-false (interval-empty? (interval 0 0 #f #f)))
  (check-false (interval-empty? (interval 1/421 1 #f #t)))

  ;; -- interval-*-open?
  (check-true (interval-lo-open? (interval 0 1 #f #f)))
  (check-true (interval-lo-open? (interval 0 1 #f #t)))
  (check-true (interval-hi-open? (interval 0 1 #f #f)))
  (check-true (interval-hi-open? (interval 0 1 #t #f)))
  (check-false (interval-lo-open? (interval 0 1 #t #f)))
  (check-false (interval-lo-open? (interval 0 1 #t #t)))
  (check-false (interval-hi-open? (interval 0 1 #f #t)))
  (check-false (interval-hi-open? (interval 0 1 #t #t)))

  ;; -- interval-disjoint?
  (check-true (interval-disjoint? (interval 0 1 #t #f) the-empty-interval))
  (check-true (interval-disjoint? the-empty-interval (interval 0 1 #t #f)))
  (check-true (interval-disjoint? (interval 0 1 #t #t) (interval 2 3 #t #t)))
  (check-true (interval-disjoint? (interval 0 1 #t #t) (interval -3 -2 #t #t)))
  (check-true (interval-disjoint? (interval 0 1 #t #f) (interval 1 3 #t #t)))
  (check-true (interval-disjoint? (interval 0 1 #t #t) (interval 1 3 #f #t)))
  (check-true (interval-disjoint? (interval 0 1 #t #f) (interval 1 3 #f #t)))

  (check-false (interval-disjoint? (interval 0 1 #f #t) (interval 1 3 #t #f)))
  (check-false (interval-disjoint? (interval 0 1 #t #f) (interval -11 0 #f #t)))
  (check-false (interval-disjoint? (interval 0 1.2 #t #f) (interval 1 3 #f #t)))
  (check-false (interval-disjoint? (interval 0 3 #t #f) (interval -1 33 #f #t)))

  ;; -- interval-intersect
  (define-syntax (check-intersects stx)
    (syntax-parse stx #:datum-literals (=>)
      [(_ [ivl1 ivl2 => res] ...)
       #'(begin (check-equal? (interval-intersect ivl1 ivl2) res) ...)]))
  (check-intersects
    [(interval 0 1 #t #t) the-empty-interval => the-empty-interval]
    [the-empty-interval (interval 0 1 #t #t) => the-empty-interval]
    [(interval 0 1 #t #t) (interval 0 1 #t #t) => (interval 0 1 #t #t)]
    [(interval 0 1 #f #t) (interval 0 1 #t #f) => (interval 0 1 #f #f)]
    [(interval 0 2 #f #t) (interval 1 3 #t #f) => (interval 1 2 #t #t)]
    [(interval 0 2 #f #f) (interval 1 3 #f #f) => (interval 1 2 #f #f)]
    [(interval 0 1 #f #f) (interval 1/3 2/3 #f #f) => (interval 1/3 2/3 #f #f)]
    [(interval 0 1 #f #f) (interval 1/3 2/3 #t #f) => (interval 1/3 2/3 #t #f)]
  )

  ;; -- interval-union
  (define-syntax (check-union stx)
    (syntax-parse stx #:datum-literals (+>)
      [(_ [ivl1 ivl2 +> res] ...)
       #'(begin (check-equal? (interval-union ivl1 ivl2) res) ...)]))
  (check-union
    [(interval 0 1 #t #t) the-empty-interval +> (interval 0 1 #t #t)]
    [the-empty-interval (interval 0 1 #t #t) +> (interval 0 1 #t #t)]
    [(interval 0 1 #t #t) (interval 0 1 #t #t) +> (interval 0 1 #t #t)]
    [(interval 0 1 #f #f)  (interval 0 1 #t #t) +> (interval 0 1 #t #t)]
    [(interval 0 1 #f #f)  (interval 0 1 #f #t) +> (interval 0 1 #f #t)]
    [(interval 0 1/2 #f #t) (interval 3 9 #f #t) +> (interval 0 9 #f #t)]
    [(interval 0 5 #t #t) (interval -10 10 #f #f) +> (interval -10 10 #f #f)]
    [(interval -10 10 #t #t) (interval 0 5 #t #t) +> (interval -10 10 #t #t)]
  )

  ;; -- interval-midpoint
  (check-equal? (interval-midpoint the-empty-interval) #f)
  (check-equal? (interval-midpoint (interval 0 2 #t #t)) 1)
  (check-equal? (interval-midpoint (interval 0 2 #f #f)) 1)
  (check-equal? (interval-midpoint (interval -1/3 1/3 #f #f)) 0)
  (check-equal? (interval-midpoint (interval 1/9 3/9 #t #t)) 2/9)
  (check-equal? (interval-midpoint (interval 1/17 3/5 #t #t)) 28/85)

  ;; -- interval-contains?
  (check-true (interval-contains? (interval 0 1 #t #t) 0))
  (check-true (interval-contains? (interval 0 1 #t #t) 1/3))
  (check-true (interval-contains? (interval 0 1 #t #t) 1/9999))
  (check-true (interval-contains? (interval 0 1 #t #t) 88/89))
  (check-true (interval-contains? (interval 0 1 #t #t) 1/1))

  (check-false (interval-contains? the-empty-interval 0))
  (check-false (interval-contains? the-empty-interval 51))
  (check-false (interval-contains? (interval 0 1 #f #t) 0))
  (check-false (interval-contains? (interval 0 1 #t #f) 1/1))

  ;; -- interval-translate
  (check-equal? (interval-translate the-empty-interval 0) the-empty-interval)
  (check-equal? (interval-translate the-empty-interval 121) the-empty-interval)
  (check-equal? (interval-translate (interval 0 10 #f #t) 4) (interval 4 14 #f #t))
  (check-equal? (interval-translate (interval 1/4 1/7 #t #f) -1/3) (interval -1/12 -4/21 #t #f))

)
