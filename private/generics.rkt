#lang racket/base

(provide
 (all-from-out (submod "." core)))

(module core racket/base
  (require racket/contract/base)
  (provide
   (except-out (all-defined-out)
	       transformable-type?
	       transformable?
	       unit-constructible-type?
	       unit-constructible?
	       bindable-type?
	       bindable?
	       joinable-type?
	       joinable?
	       extractable-type?
	       extractable?)
   (contract-out
    [transformable-type? (-> any/c boolean?)]
    [transformable? (-> any/c boolean?)]
    [unit-constructible-type? (-> any/c boolean?)]
    [unit-constructible? (-> any/c boolean?)]
    [bindable-type? (-> any/c boolean?)]
    [bindable? (-> any/c boolean?)]
    [joinable-type? (-> any/c boolean?)]
    [joinable? (-> any/c boolean?)]
    [extractable-type? (-> any/c boolean?)]
    [extractable? (-> any/c boolean?)]))

  (require
   yutes racket/generic
   (for-syntax racket/base racket/syntax syntax/parse))


  (define-syntax (define-operator stx)
    (syntax-parse stx
      [(_ op type)
       (let ([prop-name (format-syntax "prop:~a" #'op)]
	     [prop-pred? (format-syntax "~a-type?" #'type)]
	     [prop-accessor (format-syntax "~a-accessor" #'op)]
	     [getter (format-syntax "get-~a" #'op)])
	 #`(begin
	     (define-values (#,prop-name #,prop-pred? #,prop-accessor)
	       (make-struct-type-property 'op))
	     (define-generics type
	       (#,getter type)
	       #:fast-defaults
	       ([#,prop-pred?
		 (define (#,getter type)
		   ((#,prop-accessor type) type))]))))]))

  (define-operator mappend monoid-append)
  (define-operator mempty monoid-empty)
  
  (define-operator trans transformable)
  (define-operator app appable)
  (define-operator zapp zappable)
  (define-operator return unit-constructible)
  (define-operator bind bindable)
  (define-operator join joinable)
  (define-operator extract extractable)
  (define-operator extend extendable)
  (define-operator duplicate duplicatable)
  (define-operator get gettable)
  (define-operator local localable)
  (define-operator put putable)
  (define-operator op operator)
  (define-operator id identity-element)); end of submodule core


(require (submod "." core))


(module+ test
  (require  racket/function yutes rackunit)

  (struct bimonad
    (trans app zapp return bind join extract extend duplicate)
    #:property prop:trans (lambda (this) (bimonad-trans this))
    #:property prop:app (lambda (this) (bimonad-app this))
    #:property prop:zapp (lambda (this) (bimonad-zapp this))
    #:property prop:return (lambda (this) (bimonad-return this))
    #:property prop:extract (lambda (this) (bimonad-extract this))
    #:property prop:bind (lambda (this) (bimonad-bind this))
    #:property prop:extend (lambda (this) (bimonad-extend this))
    #:property prop:join (lambda (this) (bimonad-join this))
    #:property prop:duplicate (lambda (this) (bimonad-duplicate this)))

  
  (define identity-bimonad
    (bimonad call call call identity call-with identity identity call-with identity))

  (check-true (unit-constructible? identity-bimonad))
  (check-true (transformable? identity-bimonad))
  (check-true (appable? identity-bimonad))
  (check-true (zappable? identity-bimonad))
  (check-true (extractable? identity-bimonad))
  (check-true (extendable? identity-bimonad))

  (check-equal? ((get-return identity-bimonad) 1) 1)
  (check-equal? ((get-extract identity-bimonad) 1) 1)
  (check-equal? ((get-join identity-bimonad) 1) 1)
  (check-equal? ((get-duplicate identity-bimonad) 1) 1)
  
  (check-equal? ((get-trans identity-bimonad) (lambda (x) (* x x)) 3) 9)
  (check-equal? ((get-app identity-bimonad) (lambda (x) (* x x)) 3) 9)
  (check-equal? ((get-zapp identity-bimonad) (lambda (x) (* x x)) 3) 9)
  (check-equal? ((get-bind identity-bimonad) 3 (lambda (x) (* x x))) 9)
  (check-equal? ((get-extend identity-bimonad) 3 (lambda (x) (* x x))) 9))
