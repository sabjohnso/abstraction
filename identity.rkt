#lang racket/base


(provide
 (all-defined-out)
 (all-from-out "main.rkt"))

(require
 (only-in racket/function identity)
 (only-in yutes call call-with)
 "main.rkt")


(define identity-monad
  (monad call call identity call-with identity))

(define identity-comonad
  (comonad call call identity call-with identity))

(module+ test
  (require rackunit)

  (check-equal?
   (call-with identity-monad
     (let-m ([x 3]
	     [y 4])
       (return (+ x y))))
   7)

  (check-equal?
   (call-with identity-comonad
     (let-w ([x 3]
	     [y 4])
       (+ x y)))
   7)); end of submodule test
