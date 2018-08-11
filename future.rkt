#lang racket/base


(provide
 (all-defined-out)
 (all-from-out racket/future)
 (all-from-out "main.rkt"))

(require
 racket/future
 (only-in racket/function thunk)
 "main.rkt")


(define (future-fmap f mx)
  (future (thunk (f (touch mx)))))

(define (future-ap mf mx)
  (future (thunk ((touch mf) (touch mx)))))

(define (future-return x)
  (future (thunk x)))

(define (future-bind mx f)
  (future (thunk (touch (f (touch mx))))))

(define (future-join mmx)
  (future (thunk (touch (touch mmx)))))

(define future-monad
  (monad future-fmap future-ap future-return future-bind future-join))





(module+ test
  (require yutes rackunit)

  (define (sqr x) (* x x))
  
  (check-equal? (touch (future-return 'x)) 'x)
  (check-equal? (touch (future-fmap sqr (future-return 3))) 9)
  (check-equal? (touch (future-ap (future-return sqr) (future-return 3))) 9)
  (check-equal? (touch (future-bind (future-return 3) future-return)) 3)
  (check-equal? (touch (future-join (future-return (future-return 3)))) 3)

  (check-equal?
   (touch (call-with future-monad
	    (return 3)))
   3)

  
  (check-equal?
   (touch
    ((bind (pure (future-return 3))
	   return)
     future-monad))
   3)

  (check-equal?
   (touch
    (call-with future-monad
      (let-m ([x (future-return 3)]
	      [y (future-return 4)])
	(return (+ x y)))))
   7))
