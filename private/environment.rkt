#lang racket/base

(provide
 trans
 app return pure join bind
 get local put
 zapp extract duplicate duplicate* extend
 zapp*
 (struct-out abstraction-environment))

(module core racket/base

  (require racket/contract yutes "generics.rkt")
 
  (provide
   zapp*
   get
   local
   put
   
   (contract-out
    (struct abstraction-environment
      ([proc (-> any/c any/c)]))
    [trans (-> (-> any/c any/c) abstraction-environment? abstraction-environment?)]
    [app (-> abstraction-environment? abstraction-environment? abstraction-environment?)]
    [return (-> any/c abstraction-environment?)]
    [pure (-> any/c abstraction-environment?)]
    [bind (-> abstraction-environment? (-> any/c abstraction-environment?) abstraction-environment?)]
    [zapp (-> abstraction-environment? abstraction-environment? abstraction-environment?)]
    [join (-> abstraction-environment? abstraction-environment?)]
    [extract (-> abstraction-environment? any/c)]
    [extend (-> abstraction-environment? (-> abstraction-environment? any/c) abstraction-environment?)]
    [duplicate (-> abstraction-environment? abstraction-environment?)]
    [duplicate* (->  abstraction-environment? abstraction-environment?)]))

  
  (struct abstraction-environment
    (proc)
    #:property prop:procedure
    (struct-field-index proc))
  
  (define-syntax define-env
    (syntax-rules ()
      [(_ (f xs ...) (m) e)
       (define-curried (f xs ...)
	 (abstraction-environment
	  (lambda-curried (m) e)))]))

  (define-env (<op> ma mb) (m)
    ((get-op m) (ma m) (mb m)))
  
  
  (define-env (trans f mx) (m)
    ((get-trans m) f (mx m)))
 
  (define-env (app mf mx) (m)
    ((get-app m) (mf m) (mx m)))

  
  (define-env (return x) (m)
    ((get-return m) x))

  (define-env (pure mx) (m)
    mx)

  (define-env (bind mx f) (m)
    ((get-bind m) (mx m)
     (compose (call-with m) f)))

  (define-env (join mmx) (m)
    ((get-join m)
     (call-with m
       (trans (call-with m) mmx))))

  
  (define-env (get) (m)
    ((get-get m)))

  (define-env (local f mx) (m)
    ((get-local m) f (mx m)))

  (define-env (put x) (m)
    ((get-put m) x))
  

  (define-env (zapp wf wx) (w)
    ((get-zapp w) (wf w) (wx w)))

  (define zapp*
  (case-lambda
    [(fs xs)
     (zapp fs xs)]
    [(fs xs ys . zs)
     (apply zapp* (zapp fs xs) ys zs)]))

  (define-env (extract wx) (w)
    ((get-extract w)
     (wx w)))

  (define-env (extract* wx) (w)
    wx)

  (define-env (duplicate wx) (w)
    (pure ((get-duplicate w) (wx w))))

  (define-env (duplicate* wx) (w)
    ((get-duplicate w) (wx w)))

  (define-env (extend wx f) (w)
    ((get-extend w) (wx w)
      (compose (call-with w) f pure)))) ; end of submodule core

(require (submod "." core))



(module+ test
  (require
   racket/function rackunit
   yutes "structures.rkt")
  
  (define identity-monad
    (monad call call identity call-with identity))

  (define identity-comonad
    (comonad call call identity call-with identity))


  (define (sqr x) (* x x))
  (define (twc x) (+ x x))

  (check-equal?
   (call-with identity-monad
     (trans sqr (pure 3)))
   9)

  (check-equal?
   (call-with identity-monad
     (app (pure twc)  (pure 3)))
   6)

  (check-equal?
   (call-with identity-monad
     (return 3))
   3)

  (check-equal?
   (call-with identity-monad
     (bind (pure 3)
	   (compose pure sqr)))
   9)

  (check-equal?
   (call-with identity-monad
     (join (pure (pure 3))))
   3)

  (define-curried (add x y)
    (+ x y))

  (check-equal?
   (call-with identity-comonad
     (trans sqr (pure 4)))
   16)

  (check-equal?
   (call-with identity-comonad
     (zapp (trans add (pure 2))
	   (pure 3)))
   5)

  (check-equal?
   (call-with identity-comonad
     (extract (pure 4)))
   4)

  

  (check-equal?
   (call-with identity-comonad
     (extend (pure 4)
	     extract))
   4)

  (check-equal?
   (call-with identity-comonad
     (call-with identity-comonad
       (duplicate (pure 5))))
   5)

  (check-equal?
   (call-with identity-comonad
     (duplicate* (pure 5)))
   5)) ; end of submodule test

