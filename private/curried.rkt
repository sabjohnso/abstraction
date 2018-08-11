#lang racket/base



(module core racket/base
  (require
   racket/contract/base
   (only-in racket/list take drop))

  (provide
   (contract-out
    (struct partial-application ([proc procedure?] [args list?]))
    [simple-arity? (-> procedure? boolean?)]
    (struct curried-procedure ([proc (and/c procedure? simple-arity?)]))))

  (define (simple-arity? f)
    (exact-integer? (procedure-arity f)))


  (struct partial-application
    (proc args)
    #:property prop:procedure
    (lambda (this . args)
      (apply (partial-application-proc this)
	     (append (partial-application-args this)
		     args))))

  (struct curried-procedure
    (proc)
    #:transparent
    #:property prop:procedure
    (lambda (this . args)
      (let* ([proc (curried-procedure-proc this)]
	     [arity (procedure-arity proc)])
	(cond [(= (length args) arity) (apply proc args)]
	      [(< (length args) arity) (partial-application this args)]
	      [(> (length args) arity)
	       (apply (apply (curried-procedure-proc this) (take args arity))
		      (drop args arity))]))))) ; end of submodule core

(require
 (for-syntax racket/base racket/syntax syntax/parse)
 (submod "." core))


(define-syntax (lambda-curried stx)
  (syntax-parse stx
    [(_ (xs ...) es ...+)
     #'(curried-procedure (lambda (xs ...) es ...))]))


(define-syntax (define-curried stx)
  (syntax-parse stx
    [(_ (f:id xs:id ...) es:expr ...+) #'(define f (lambda-curried (xs ...) es ...))]
    [(_ f:id e:expr) #'(define f (curried-procedure e))]))


(define (make-curried f)
  (curried-procedure f))



(module+ test
  (require (only-in racket/function thunk) rackunit)



  

 

  )
