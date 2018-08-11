#lang racket/base

(require racket/contract)

(provide
 (all-defined-out)
 (all-from-out "main.rkt"))

(module core racket/base
  (require yutes racket/function racket/contract "tools.rkt")
  (provide
   call/cc-m
   (contract-out
    (struct cont ([proc (-> (-> any/c any/c) any/c)]))
    [trans  trans/c]
    [app    app/c]
    [return return/c]
    [bind   bind/c]
    [join   join/c]))

  (struct cont
    (proc)
    #:transparent
    #:property prop:procedure
    (struct-field-index proc))

  (define-values (cont->cont/c constructor->cont/c trans/c app/c return/c bind/c join/c)
    (make-monad-contracts cont?))
  


  (define-syntax define-cont
    (syntax-rules ()
      [(_ (name args ...) (k) e)
       (define-curried (name args ...)
	 (cont (lambda (k) e)))]))

  (define-cont (return x) (k)
    (k x))

  (define-cont (bind mx f) (k)
    ((f (mx k)) k))

  (define-values (trans app join)
    (return/bind->monad return bind))

  (define call/cc-m/c
    (case->
     (-> (-> any/c any/c) cont?)
     (-> (recursive-contract call/cc-m/c))))

  (define-cont (call/cc-m f) (k)
    (f k))); end of submodule core


(require
 (prefix-in cont- (except-in (submod "." core) cont))
 (only-in (submod "." core) cont)
 "main.rkt")


(define cont-monad
  (monad cont-trans cont-app cont-return cont-bind cont-join))


(define-syntax lambda-cps
  (syntax-rules ()
    [(_ (x) e es ...)
     (lambda (x)
       ((lambda (r)
	  (lambda (k)
	    (k r))) (begin e es ...)))]
    
    [(_ (x y zs ...) e es ...)
     (lambda (x) (lambda-cps (y zs ...) e es ...))]))

(define-syntax define-cps
  (syntax-rules ()
    [(_ (f x xs ...) e es ...)
     (define f
       (lambda-cps (x xs ...) e es ...))]))

(define ($ f x . xs)
  (if (null? xs)
      (f x)
      (apply $ (f x) xs)))


(module+ test
  (require yutes rackunit "list.rkt")
  
  (define-curried (validate name escape)
    (when (or (not (string? name)) (string=? name ""))
      (escape "invalid name!!!")))

  
  (define (say-hello-to name)
    (call-with cont-monad
      (begin-m
	(cont (validate name))
	(return (format "Hello, ~a"
			name)))))

  (define (@ mx)
    (cont (lambda (k) (bind mx k))))



  ); end of submodule test
