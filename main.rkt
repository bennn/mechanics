#lang racket/base

;; 2015-04-27:
;; Macros for the SICM library.
;; Placed here (as 'main.rkt' in the top directory) for convenience developing.

;; The contents of this file will eventually live somewhere else,
;; and 'main.rkt' will just be the library's API.

(provide
 provide/api
 ;; (provide (ID CONTRACT DOC) ...)
 ;; An optional `provide` form for identifiers that requires a contract and
 ;;  docstring. Desugars to a `contract-out` for the identifier.
 (all-from-out racket/contract)
 ;; Re-export `racket/contract`, for `provide/api` to work.
)

;; -----------------------------------------------------------------------------

(require
 racket/contract
 (for-syntax
  racket/base
  syntax/parse
  (only-in unstable/sequence in-syntax)))

;; =============================================================================

(define-syntax (provide/api stx)
  (syntax-parse stx
    [(_ (~seq pr*:id
              #:contract ctr*
              #:doc      doc*:str) ...)
     ;; Ignores the docstring for now.
     ;;  should check if compiling in documentation mode & save doc.
     ;; Also, a "no contract" form/flag would be nice.
     #'(provide (contract-out [pr* ctr*] ...))]
    [_ (error (format "provide/api: syntax error, expected '(provide/api (ID CONTRACT DOC) ...)' but given '~a'" (syntax->datum stx)))]))
