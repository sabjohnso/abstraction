#lang racket/base

(provide (all-defined-out))

(require
 racket/contract
 (only-in racket/function identity)
 yutes)

(define (return/bind->monad return bind)
  (define-curried (trans f mx)
    (bind mx (compose return f)))
  
  (define-curried (app mf mx)
    (bind mf (lambda (f)
	       (bind mx (lambda (x)
			  (return (f x)))))))
  
  (define-curried (join mmx)
    (bind mmx identity))
  
  (values trans app join))



(define (trans/return/join->monad trans return join)
  (define-curried (app mf mx)
    (join  (trans (lambda (f) (trans f mx)) mf)))  
  (define-curried (bind mx f)
    (join (trans f mx)))
  (values app bind))

(define-syntax define->/c
  (syntax-rules ()
    [(_ name /c)
     (define name
       (case->
	(-> /c /c)
	(-> (recursive-contract name))))]))

(define-syntax define-constructor->/c
  (syntax-rules ()
    [(_ name /c)
     (define name
       (-> (-> any/c /c) /c))]))

(define-syntax define-trans/c
  (syntax-rules ()
    [(_ name /c ->/c)
     (define name
       (case->
	(-> (-> any/c any/c) /c /c)
	(-> (-> any/c any/c) (-> ->/c))
	(-> (recursive-contract name))))]))

(define-syntax define-app/c
  (syntax-rules ()
    [(_ name /c ->/c)
     (define name
       (case->
	(-> /c /c /c)
	(-> /c (-> ->/c))
	(-> (recursive-contract name))))]))

(define-syntax define-return/c
  (syntax-rules ()
    [(_ name /c)
     (define name
       (case->
	(-> any/c /c)
	(-> (recursive-contract name))))]))

(define-syntax define-bind/c
  (syntax-rules ()
    [(_ name /c constructor->/c)
     (define name
       (case->
	(-> /c (-> any/c /c) /c)
	(-> /c (-> constructor->/c))
	(-> (recursive-contract name))))]))

(define-syntax define-join/c
  (syntax-rules ()
    [(_ name /c)
     (define name
       (case->
	(-> /c /c)
	(-> (recursive-contract name))))]))

(define (make-monad-contracts ?)
  (define->/c ?->?/c ?)
  (define-constructor->/c constructor->?/c ?)
  (define-trans/c trans/c ? ?->?/c)
  (define-app/c app/c ? ?->?/c)
  (define-return/c return/c ?)
  (define-bind/c bind/c ? constructor->?/c)
  (define-join/c join/c ?)
  (values ?->?/c constructor->?/c
	  trans/c app/c return/c bind/c join/c))





