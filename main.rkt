#lang racket/base


(provide
 (all-from-out "private/syntax.rkt")
 (all-from-out "private/generics.rkt")
 (all-from-out "private/structures.rkt")
 lift-m lift-m2 lift-m3 lift-m4 lift-m5
 lift-w lift-w2 lift-w3 lift-w4 lift-w5
 select
 modify)

(require
 yutes
 "private/syntax.rkt"
 "private/generics.rkt"
 "private/structures.rkt")


(define-curried (lift-m f ma)
  (let-m ([a ma])
    (return (f a))))

(define-curried (lift-m2 f ma mb)
  (let-m ([a ma]
	  [b mb])
    (return (f a b))))

(define-curried (lift-m3 f ma mb mc)
  (let-m ([a ma]
	  [b mb]
	  [c mc])
    (return (f a b c))))

(define-curried (lift-m4 f ma mb mc md)
  (let-m ([a ma]
	  [b mb]
	  [c mc]
	  [d md])
    (return (f a b c d))))


(define-curried (lift-m5 f ma mb mc md me)
  (let-m ([a ma]
	  [b mb]
	  [c mc]
	  [d md]
	  [e me])
    (return (f a b c d e))))



(define-curried (lift-w f wa)
  (let-w ([a wa])
    (f a)))

(define-curried (lift-w2 f wa wb)
  (let-w ([a wa]
	  [b wb])
    (f a b)))


(define-curried (lift-w3 f wa wb wc)
  (let-w ([a wa]
	  [b wb]
	  [c wc])
    (f a b c)))

(define-curried (lift-w4 f wa wb wc wd)
  (let-w ([a wa]
	  [b wb]
	  [c wc]
	  [d wd])
    (f a b c d)))

(define-curried (lift-w5 f wa wb wc wd we)
  (let-w ([a wa]
	  [b wb]
	  [c wc]
	  [d wd]
	  [e we])
    (f a b c d e)))

(define-curried (select f)
  (let-m ([x (get)])
    (return (f x))))

(define-curried (modify f)
  (let-m ([x (get)])
    (put (f x))))





