#lang racket/base

(module core racket/base
  (require yutes racket/vector racket/contract/base "tools.rkt")
  
  (provide
   (contract-out
    [trans  trans/c]
    [app    app/c]
    [return return/c]
    [bind   bind/c]
    [join   join/c]))

  (define-curried (trans f xs)
    (vector-map f xs))

  (define-curried (return x)
    (vector x))

  (define-curried (join xss)
    (apply vector-append (vector->list xss)))

  (define-values (app bind) (trans/return/join->monad trans return join))

  (define-values
      (vector->vector/c constructor->vector/c trans/c app/c return/c bind/c join/c)
    (make-monad-contracts vector?))); end of submodule core


(require
 (prefix-in vector- (submod "." core))
 "main.rkt")

(define vector-monad
  (monad vector-trans vector-app vector-return vector-bind vector-join))

(module+ test
  (require yutes rackunit)

  (define (twc x) (+ x x))

  (check-equal?
   (vector-trans twc #(1 2 3))
   #(2 4 6))

  (check-equal?
   (vector-app (vector twc) #(1 2 3))
   #(2 4 6))

  (check-equal?
   (vector-return 3)
   #(3))

  (check-equal?
   (vector-bind #(1 2 3) vector)
   #(1 2 3))

  (check-equal?
   (vector-join #(#(1) #(2) #(3)))
   #(1 2 3))

  (check-equal?
   (call-with vector-monad
     (let-m ([x <~ #(1 2)]
	     [y <~ #(3 4)])
       (return (+ x y))))
   #(4 5 5 6))


  (check-equal?
   (call-with vector-monad
     (let-m ([x <~ #(1 2)]
	     [y <= 3])
       (return (+ x y))))
   #(4 5))) ; end of submodule test





