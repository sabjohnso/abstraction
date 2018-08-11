#lang racket/base

(provide
 maybe-monad
 just nothing
 (all-from-out "main.rkt"))

(module core racket/base
  (require racket/contract/base)

  (provide
   (contract-out
    (struct just ([value any/c]))
    (struct nothing ())
    [trans  trans/c]
    [app    app/c]
    [return return/c]
    [bind   bind/c]
    [join   join/c]))
  
  (require
   yutes racket/match
   (only-in racket/function identity)
   "tools.rkt")
  
  
  (struct maybe () #:transparent)
  (struct nothing maybe () #:transparent)
  (struct just maybe (value) #:transparent)

  (define-curried (return x)
    (just x))

  (define-curried (bind mx f)
    (match mx
      [(just x) (f x)]
      [(nothing) (nothing)]))

  (define-values (trans app join)
    (return/bind->monad return bind))
  
  (define-values (maybe->maybe/c constructor->maybe/c trans/c app/c return/c bind/c join/c)
    (make-monad-contracts maybe?))) ; end of submodule core



(require
 (prefix-in maybe- (except-in (submod "." core) just nothing)) 
 (only-in (submod "." core) just nothing)
 "main.rkt")



(define maybe-monad
  (monad maybe-trans maybe-app maybe-return maybe-bind maybe-join))







(module+ test
  (require yutes racket/contract rackunit)
  
  (contract-exercise maybe-trans maybe-app maybe-return maybe-bind maybe-join)

  (check-equal? (get-trans maybe-monad) maybe-trans)
  (check-equal? (get-app maybe-monad) maybe-app)
  (check-equal? (get-return maybe-monad) maybe-return)
  (check-equal? (get-bind maybe-monad) maybe-bind)
  (check-equal? (get-join maybe-monad) maybe-join)
  
  

  (define (twc x) (+ x x))

  (check-equal?
   (maybe-trans twc (just 3))
   (just 6))

  (check-equal?
   (maybe-app (just twc) (just 3))
   (just 6))

  (check-equal?
   (maybe-app (nothing) (just 3))
   (nothing))

  (check-equal?
   (maybe-app (just twc) (nothing))
   (nothing))

  (check-equal?
   (maybe-app (nothing) (nothing))
   (nothing))

  (check-equal?
   (maybe-return 3)
   (just 3))
  
  (check-equal?
   (maybe-bind (maybe-return 3) maybe-return)
   (just 3))


  (define (rcp x)
    (if (zero? x) (nothing)
	(just (/ x))))

  (check-equal?
   (call-with maybe-monad
     (let-m ([x <~ (rcp 3)]
	     [y <- (return 3)])
       (return (+ x y))))
   (just 10/3))

  (check-equal?
   (call-with maybe-monad
     (let-m ([x <~ (rcp 0)]
	     [y <- (return 3)])
       (return (+ x y))))
   (nothing))) ; end of submodule test
