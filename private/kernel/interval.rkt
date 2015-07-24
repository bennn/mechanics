#lang racket/base

;; Utilities for working with subsets of the real line.

;; The `plot-lib` library's `common/math.rkt` gives a similar abstraction

(provide
  ;; Constructor
  (rename-out
    [make-interval/check interval]
    [make-interval/check make-interval])
  ivl
  ;; Accessors
  interval-lo
  interval-hi
  interval-lo-closed?
  interval-hi-closed?
)

;; -----------------------------------------------------------------------------

(require
  mechanics
  (for-syntax racket/base syntax/parse)
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
  (if (or (> lo hi)
          (and (equal? lo hi) (not (or lo-closed? hi-closed?))))
    the-empty-interval
    (interval lo hi lo-closed? hi-closed?)))

;; We could possibly hack the #lang to distinguish square braces from parens.
;; (#lang mechanics)
;; TODO make sure unicode display correctly
;; TODO use unicode << and [[ instead (double-angle and double-brace)
(define-syntax (ivl stx)
  ;; Do NOT check `lo:number`, let `make-interval/check` test & raise the error
  (syntax-parse stx #:datum-literals (â¨ â© âª â«)
    [(_ â¨ lo , hi â©)
     #'(make-interval/check lo hi #f #f)]
    [(_ â¨ lo , hi â«)
     #'(make-interval/check lo hi #f #t)]
    [(_ âª lo , hi â©)
     #'(make-interval/check lo hi #t #f)]
    [(_ âª lo , hi â«)
     #'(make-interval/check lo hi #t #t)]
    [_ (error 'interval "Could not parse syntax, expected an interval delimited by angle braces, got '~a'\n" stx)]))

;; =============================================================================



;; =============================================================================

(module+ test
  (require rackunit)

  ;; Constructors
  (check-equal? (ivl â¨ 0 , 1 â©) (interval 0 1 #f #f))
  (check-equal? (ivl â¨ 0 , 1 â«) (interval 0 1 #f #t))
  (check-equal? (ivl âª 0 , 1 â©) (interval 0 1 #t #f))
  (check-equal? (ivl âª 0 , 1 â«) (interval 0 1 #t #t))

  (check-equal? (ivl â¨ 0 , 0 â©) the-empty-interval)
  (check-equal? (ivl â¨ 0 , 0 â«) (interval 0 0 #f #t))
  (check-equal? (ivl âª 0 , 0 â©) (interval 0 0 #t #f))
  (check-equal? (ivl âª 0 , 0 â«) (interval 0 0 #t #t))
  (check-equal? (ivl â¨ 1 , 0 â©) the-empty-interval)

)
