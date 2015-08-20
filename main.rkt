#lang racket/base

;; 2015-04-27:
;; Macros for the SICM library.
;; Placed here (as 'main.rkt' in the top directory) for convenience developing.

;; The contents of this file will eventually live somewhere else,
;; and 'main.rkt' will just be the library's API.

(provide
  provide/api
  ;; An optional `provide` form for identifiers that requires a contract and
  ;;  docstring. Desugars to a `contract-out` for the identifier.
  ;; (provide/api
  ;;   ID #:contract CONTRACT #:doc DOCSTRING ...)

  (all-from-out racket/contract/base)
  ;; Re-export, for `provide/api` to work.

  exp2 exp10
  ;; (expN m) computes (expt N m)

  Sigma Σ
  ;; Generalized sum.
  ;; `(Sigma f lo hi)` sums `(f i)` for all `i` between `lo` and `hi`, inclusive

  Pi Π
  ;; Generalized product.
  ;; `(Pi f lo hi)` multiplies `(f i)` for all `i` between `lo` and `hi`, inclusive
)

;; -----------------------------------------------------------------------------

(require
 racket/contract/base
 (for-syntax
  racket/base
  syntax/parse
  (only-in unstable/sequence in-syntax)))

;; =============================================================================

(define-syntax (provide/api stx)
  (syntax-parse stx
    [(_ (~seq pr*:id
              #:contract (~optional enabled?*:boolean #:defaults ([enabled?* #'#t])) ctr*
              #:doc      doc*:str) ...)
     ;; Ignores docstrings
     #`(provide
         #,@(for/list ([pr (in-list (syntax->list #'(pr* ...)))]
                       [enabled? (in-list (syntax->list #'(enabled?* ...)))]
                       [ctr (in-list (syntax->list #'(ctr* ...)))])
              ;; Skip the contract if given `#:contract #f ctr`
              (if (syntax-e enabled?) #`(contract-out [#,pr #,ctr]) #`#,pr)))]
    [_ (error (format "provide/api: syntax error, expected '(provide/api (ID #:contract CONTRACT #:doc DOCSTRING) ...)' but given '~a'" (syntax->datum stx)))]))

;; =============================================================================

(define (exp2 n)
  (expt 2 n))

(define (exp10 n)
  (expt 10 n))

(define (Sigma f lo hi)
  (for/sum ([i (in-range lo (add1 hi))])
    (f i)))
(define Σ Sigma)

(define (Pi f lo hi)
  (for/product ([i (in-range lo (add1 hi))])
    (f i)))
(define Π Pi)

;; =============================================================================

(module+ test
  (require rackunit)

  (define-syntax-rule (exp2n=expt2n [n ...])
    (begin (check-equal? (exp2 n) (expt 2 n)) ...))
  (exp2n=expt2n [-2 -1 0 1 2 3 4])

  (define-syntax-rule (exp10n=expt10n [n ...])
    (begin (check-equal? (exp10 n) (expt 10 n)) ...))
  (exp10n=expt10n [-2 -1 0 1 2 3 4])

  (check-equal? (Sigma (lambda (x) x) 1 10) (+ 1 2 3 4 5 6 7 8 9 10))
  (check-equal? (Sigma (lambda (x) x) 0 0) 0)
  (check-equal? (Sigma (lambda (x) x) 1 1) 1)
  (check-equal? (Sigma (lambda (x) x) 1 0) 0)
  (check-equal? (Sigma (lambda (x) (+ 2 x)) 10 12) (+ 12 13 14))

  (check-equal? (Pi (lambda (x) x) 1 10) (* 1 2 3 4 5 6 7 8 9 10))
  (check-equal? (Pi (lambda (x) x) 0 0) 0)
  (check-equal? (Pi (lambda (x) x) 2 2) 2)
  (check-equal? (Pi (lambda (x) x) 2 1) 1)
  (check-equal? (Pi (lambda (x) 4) 0 4) (expt 4 5))
)
