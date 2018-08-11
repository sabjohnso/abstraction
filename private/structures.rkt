#lang racket/base

(provide
 (struct-out monoid)
 (struct-out functor)
 (struct-out applicative)
 (struct-out monad)
 (struct-out monad-environment)
 (struct-out monad-state)
 (struct-out co-applicative)
 (struct-out comonad))

(module core racket/base

  (require
   racket/contract
   "generics.rkt")
  
  (provide
   (contract-out
    (struct monoid
      ([op (-> any/c any/c any/c)]
       [id any/c]))
    
    (struct functor
      ([trans (-> (-> any/c any/c) any/c any/c)]))
    
    (struct applicative
      ([trans (-> (-> any/c any/c) any/c any/c)]
       [app (-> any/c any/c any/c)]
       [return (-> any/c any/c)]))
    
    (struct monad
      ([trans (-> (-> any/c any/c) any/c any/c)]
       [app (-> any/c any/c any/c)]
       [return (-> any/c any/c)]
       [bind (-> any/c (-> any/c any/c) any/c)]
       [join (-> any/c any/c)]))

    (struct monad-environment
      ([trans  (-> (-> any/c any/c) any/c any/c)]
       [app    (-> any/c any/c any/c)]
       [return (-> any/c any/c)]
       [bind   (-> any/c (-> any/c any/c) any/c)]
       [join   (-> any/c any/c)]
       [get    (-> any/c)]
       [local  (-> (-> any/c any/c) any/c any/c)]))

    (struct monad-state
      ([trans  (-> (-> any/c any/c) any/c any/c)]
       [app    (-> any/c any/c any/c)]
       [return (-> any/c any/c)]
       [bind   (-> any/c (-> any/c any/c) any/c)]
       [join   (-> any/c any/c)]
       [get    (-> any/c)]
       [local  (-> (-> any/c any/c) any/c any/c)]
       [put    (-> any/c any/c)]))

    (struct co-applicative
      ([trans (-> (-> any/c any/c) any/c any/c)]
       [zapp (-> any/c any/c any/c)]
       [extract (-> any/c any/c)]))

    (struct comonad
      ([trans (-> (-> any/c any/c) any/c any/c)]
       [zapp (-> any/c any/c any/c)]
       [extract (-> any/c any/c)]
       [extend (-> any/c (-> any/c any/c) any/c)]
       [duplicate (-> any/c any/c)]))))

  (struct monoid
    (op id)
    #:property prop:op (lambda (this) (monoid-op this))
    #:property prop:id (lambda (this) (monoid-id this))
    #:transparent)

  (struct functor
    (trans)
    #:property prop:trans (lambda (this) (functor-trans this)))

  (define functorial-type? transformable-type?)

  (struct applicative
    functor
    (app return)
    #:property prop:app (lambda (this) (applicative-app this))
    #:property prop:return (lambda (this) (applicative-return this)))

  (struct monad
    applicative
    (bind join)
    #:property prop:bind (lambda (this) (monad-bind this))
    #:property prop:join (lambda (this) (monad-join this)))



  
  (struct monad-environment
    monad
    (get local)
    #:property prop:get
    (lambda (this)
      (monad-environment-get this))
    
    #:property prop:local
    (lambda (this)
      (monad-environment-local this)))

  (struct monad-state
    monad-environment
    (put)
    #:property prop:put (lambda (this) (monad-state-put this)))

  (struct co-applicative
    functor
    (zapp extract)
    #:property prop:zapp (lambda (this) (co-applicative-zapp this))
    #:property prop:extract (lambda (this) (co-applicative-extract this)))
  
  (struct comonad
    co-applicative
    (extend duplicate)
    #:property prop:extend (lambda (this) (comonad-extend this))
    #:property prop:duplicate (lambda (this) (comonad-duplicate this)))) ; end of submodule core


(require (submod "." core))

(module+ test
  (require rackunit)) ; end of submodule test
