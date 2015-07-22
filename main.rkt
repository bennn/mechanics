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
 (all-from-out racket/contract/base)
 ;; Re-export, for `provide/api` to work.
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
    [_ (error (format "provide/api: syntax error, expected '(provide/api (ID CONTRACT DOC) ...)' but given '~a'" (syntax->datum stx)))]))

