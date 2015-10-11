#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                                  EXPRESS                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Manipulations for symbolic expressions
;;

(require
 (only-in mechanics/private/kernel/generics gen:typed-object type)
 (only-in mechanics/private/kernel/types number-type-tag)
 racket/contract/base)

(provide
 (contract-out
  [struct symbolic-expression ((properties hash?))]

  [make-symbolic-expression (->* () ()
                                 #:rest symbolic-expression-kv-pairs/c
                                 symbolic-expression?)]
  
  [expression-property (-> symbol? symbolic-expression? any)]
  
  [get-property (-> symbolic/c symbol? any/c any)]
  
  [has-property? (-> symbol?
                     (-> (or/c symbolic-expression? symbol?)
                         (list/c symbol? any/c)))]

  [add-property! (-> symbolic-expression? symbol? any/c void?)]

  [add-property (-> symbolic-expression? symbol? any/c symbolic-expression?)]

  [expression-of (-> symbolic/c any)]
  
  ;; TODO: make a type tag predicate
  [make-literal (-> symbol? symbolic/c symbolic-expression?)]

  [make-numerical-literal (-> symbolic/c symbolic-expression?)]
  
  [make-real-literal (-> symbolic/c symbolic-expression?)]

  [make-combination (-> symbol?
                        symbol?
                        (listof symbolic/c)
                        symbolic-expression?)]
  
  [operator (-> (or/c symbolic-expression? (listof symbolic/c)) symbol?)]
  
  [operands (-> (or/c symbolic-expression? (listof symbolic/c)) symbol?)]
  
  [first-operand (-> (or/c symbolic-expression? (listof symbolic/c))
                     (or/c symbol? (listof symbolic/c)))]
  
  [second-operand (-> (or/c symbolic-expression? (listof symbolic/c))
                      (or/c symbol? (listof symbolic/c)))]
  
  [rest-operands (-> (or/c symbolic-expression? (listof symbolic/c))
                     (or/c symbol? (listof symbolic/c)))]

  [up-constructor-name symbol?]
  
  [down-constructor-name symbol?]

  [row-maker? (-> any/c boolean?)]

  [column-maker? (-> any/c boolean?)]

  [vector-maker? (-> any/c boolean?)]

  [quaternion-maker? (-> any/c boolean?)]

  [matrix-by-rows-maker? (-> any/c boolean?)]

  [matrix-by-columns-maker? (-> any/c boolean?)]

  [matrix-maker? (-> any/c boolean?)]

  [compound-data-constructor? (-> any/c boolean?)]

  ))

;; symbolic expressions are hash-tables of properties
(struct symbolic-expression (properties)
  #:methods gen:typed-object
  [(define (type expr)
     (expression-property 'type expr))
   (define (type-predicate expr)
     (expression-property 'type-predicate expr))])

(define (make-symbolic-expression . bindings)
  (symbolic-expression (apply hasheq bindings)))

(define symbolic/c (or/c symbol? symbolic-expression?))

(define symbolic-expression-kv-pairs/c
  (flat-named-contract
   'symbolic-expression-kv-pair
   (λ args
     (for/and ([i (in-range (length args))]
               #:when (even? i))
       (symbol? (list-ref args i))))))

(define (expression-property property-name expr)
  (hash-ref (symbolic-expression-properties expr)
            property-name
            #f))

(define (make-literal type-tag expression)
  (make-symbolic-expression
   'type type-tag
   'expression expression))

(define (operator expr)
  (cond
    [(symbolic-expression? expr)
     (car (expression-property 'expression expr))]
    [(list? expr)
     (car expr)]))

(define (operands expr)
  (cond
    [(symbolic-expression? expr)
     (cdr (expression-property 'expression expr))]
    [(list? expr)
     (cdr expr)]))

(define (first-operand expr)
  (cond
    [(symbolic-expression? expr)
     (cadr (expression-property 'expression expr))]
    [(list? expr)
     (cadr expr)]))

(define (second-operand expr)
  (cond
    [(symbolic-expression? expr)
     (caddr (expression-property 'expression expr))]
    [(list? expr)
     (cadr expr)]))

(define (rest-operands expr)
  (cond
    [(symbolic-expression? expr)
     (cddr (expression-property 'expression expr))]
    [(list? expr)
     (cddr expr)]))

(define (substitute v x expr)
  (if (equal? v x)
      expr
      (let loop ([e expr])
        (cond
          [(equal? v x)
           v]

          [(pair? e)
           (cons (loop (car e))
                 (loop (cdr e)))]

          [(vector? e)
           (for/vector ([sub-expr (in-vector e)])
             (loop  e))]

          [else
           e]))))

(define ((has-property? property-name) expr)
  (cond
    [(symbolic-expression? expr)
     (list property-name (expression-property property-name expr))]
    [(symbol? expr)
     (if (eq? property-name 'expression)
         (list 'expression expr)
         (error "Bad abstract quantity" expr))]))

(define (get-property expr property-name [default #f])
  (cond
    [(symbolic-expression? expr)
     (let ([property-value (expression-property property-name expr)])
       (or property-value default))] ))

(define (add-property! expr property-name property-value)
  (hash-set! (symbolic-expression-properties expr)
             property-name
             property-value))

(define (add-property expr property-name property-value)
  (add-property! expr property-name property-value)
  expr)

(define (make-numerical-literal expr)
  (make-literal number-type-tag expr))

(define (make-real-literal expr)
  (let ([numeric-exp (make-numerical-literal expr)])
    (add-property! numeric-exp 'real #t)
    numeric-exp))

(define (make-combination type-tag operator operands)
  (make-literal type-tag (cons operator operands)))

(define (expression-of expr)
  (cond
    [(symbolic-expression? expr)
     (let ([content-expression (expression-property 'expression expr)])
       (if content-expression
           (type content-expression)
           (error "No expression for abstract quantity." expr)))]
    
    [(symbol? expr)
     expr]))

;; In this system, expressions never contain vectors or matrices,
;; they only contain constructions for them.  Thus we need to be able
;; to recognize the constructors:

(define up-constructor-name 'up)
(define down-constructor-name 'down)

(define-syntax-rule (define/symbolic-expression-predicate name type)
  (define (name expr)
    (and (symbolic-expression? expr)
         (eq? (type expr) type))))

(define/symbolic-expression-predicate row-maker? down-constructor-name)
(define/symbolic-expression-predicate column-maker? up-constructor-name)
(define/symbolic-expression-predicate vector-maker? 'vector)
(define/symbolic-expression-predicate quaternion-maker? 'quaternion)
(define/symbolic-expression-predicate matrix-by-rows-maker? 'matrix-by-rows)
(define/symbolic-expression-predicate matrix-by-columns-maker? 'matrix-by-columns)

(define (matrix-maker? expr)
  (or (matrix-by-rows-maker? expr)
      (matrix-by-columns-maker? expr)))

(define (compound-data-constructor? expr)
  (and (symbolic-expression? expr)
       (memq (type expr)
	     '(list
	       vector
	       quaternion
	       row
	       column
	       up 
	       down
	       matrix-by-rows
	       matrix-by-cols))))

(define heuristic-number-canonicalizer (make-parameter #f))

;; Break up the expression construction into the separate cases

(define (number->symbolic-expression n)
  (parameterize ([heuristic-number-canonicalizer
                  (heuristic-number-canonicalizer)])
    `,(if (and (inexact? n) heuristic-number-canonicalizer)
          (heuristic-number-canonicalizer n)
          n)))

(define (differential->symbolic-expression diff)
  ;; TODO: implement all the free variables in here
  `(make-differential-quantity
    ;; Is this `list` necessary?
    (list ,@(map (λ (term)
                   `(make-differential-term
                     ',(differential-tags term)
                     ,(expression (differential-coefficient term))))
                 (differential-term-list diff)))))

;; TODO: figure this out
(define (expression expr)
  (cond
    [(or (null? expr) (symbol? expr))
     expr]
    
    [(number? expr)
     (number->symbolic-expression expr)]

    [(differential? expr)
     (differential->symbolic-expression)]))

;; TODO functions
(define (differential? expr)
  (error "TODO: differential?"))

(define (differential-tags . args)
  (error "TODO: differential-tags"))

(define (differential-coefficient . args)
  (error "TODO: differential-coefficient"))

(define (differential-term-list . args)
  (error "TODO: differential-coefficient"))
