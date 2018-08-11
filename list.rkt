#lang racket/base

(provide
 list-monad
 (all-from-out "main.rkt"))

(module core racket
  (require racket/contract)
  

  (provide
   (contract-out
    [list->list/c contract?]
    [trans/c contract?]
    [app/c contract?]
    [return/c contract?]
    [bind/c contract?]
    [join/c contract?]

    [extract/c contract?]
    [extend/c contract?]
    [duplicate/c contract?]
    

    [trans trans/c]
    [app app/c]    
    [return return/c]
    [bind bind/c]
    [join join/c]

    [zapp app/c]
    [extract extract/c]
    [extend extend/c]
    [duplicate duplicate/c]

    [monoid struct/monoid?]
    [monad struct/monad?]
    [comonad struct/comonad?]))


  (require yutes (prefix-in struct/ "private/structures.rkt"))
  
  (define list->list/c (-> list? list?))

  (define trans/c
    (case->
     (-> (-> any/c any/c) list? list?)
     (-> (-> any/c any/c) list->list/c)
     (-> (recursive-contract trans/c))))

  (define app/c
    (case->
     (-> (listof (-> any/c any/c)) list? list?)
     (-> (listof (-> any/c any/c)) list->list/c)
     (-> (recursive-contract app/c))))

  (define return/c
    (->i ([x any/c]) ()
	 [xs (x) (lambda (xs) (and (list? xs) (= (length xs) 1)
				   (equal? (car xs) x)))]))

  (define bind/c
    (case->
     (-> list? (-> any/c list?) list?)
     (-> list? (-> (-> any/c list?) list?))
     (-> (recursive-contract bind/c))))

  (define-curried (length=/c xs ys)
    (and (list? xs)
      (list? ys)
      (= (length xs) (length ys))))

  (define join/c
    (case->
     (-> (listof list?) list?)
     (-> (recursive-contract join/c))))
  
  (define extract/c
    (case->
     (-> (non-empty-listof any/c) any/c)
     (-> (recursive-contract extract/c))))

  (define extend/c
    (case->
     (-> (non-empty-listof any/c) (-> any/c any/c) (non-empty-listof any/c))
     (-> (non-empty-listof any/c) (-> (-> any/c any/c) (non-empty-listof any/c)))
     (-> (recursive-contract extend/c))))
  
  (define duplicate/c
    (case->
     (-> (non-empty-listof any/c) (non-empty-listof list?))
     (-> (recursive-contract duplicate/c))))
  

  (define-curried (trans f xs)
    (map f xs))

  

  (define-curried (app fs xs)
    (join (trans (lambda (f) (trans f xs)) fs)))
  

  (define-curried (return x)
    (list x))

  (define-curried (bind xs f)
    (join (map f xs)))

  (define-curried (join xss)
    (apply append xss))

  (define-curried (zapp fs xs)
    (map call fs xs))
  
  (define-curried (extract xs)
    (car xs))

  (define-curried (extend xs f)
    (trans f (duplicate xs)))

  
  (define-curried (duplicate xs)
    (for/list ([i (in-range (length xs))])
      (append (drop xs i)
	      (take xs i))))
  

  (define monoid
    (struct/monoid append '()))

  (define monad
    (struct/monad trans app return bind join))

  (define comonad
    (struct/comonad trans zapp extract extend duplicate))) ; end of submodule core


(require
 (prefix-in list- (submod "." core))
 "main.rkt")





(module+ test
  (require "private/generics.rkt" yutes racket/contract racket/pretty rackunit)

  (contract-exercise
   list-trans
   list-app
   list-return
   list-bind
   list-join)

  (check-equal? (get-op list-monoid) append)
  (check-equal? (get-id list-monoid) '())
  (check-equal? (get-trans list-monad) list-trans)
  (check-equal? (get-app list-monad) list-app)
  (check-equal? (get-return list-monad) list-return)
  (check-equal? (get-bind list-monad) list-bind)
  (check-equal? (get-join list-monad) list-join)
  
  (prog
   (define (sqr x) (* x x))
   (define (twc x) (+ x x))

   (check-equal?
    (list-trans sqr '(1 2 3 4))
    '(1 4 9 16))

   (check-equal?
    (list-app (list sqr twc) '(1 2 3 4))
    '(1 4 9 16 2 4 6 8))

   (check-equal?
    (list-return 'x)
    '(x))

   (check-equal?
    (list-bind '(1 2 3 4) list-return)
    '(1 2 3 4))

   (check-equal?
    (list-bind '(1 2 3 4) (lambda (x) (if (> x 2) (list x) '())))
    '(3 4))

   
   (check-equal?
    (call-with list-monad
      (let-m ([x '(1 2)]
	      [y '(3 4)])
	(return (+ x y))))
    '(4 5 5 6))

   (check-equal?
    (call-with list-monad
      (trans sqr (pure '(1 2))))
    '(1 4)))

  
  (check-equal?
   (list-zapp
    (list-trans (lambda-curried (x y) (list x y)) '(1 2))
    '(3 4))
   '((1 3) (2 4)))

  (check-equal?
   (call-with list-comonad
     (zapp
      (trans (lambda-curried (x y) (list x y)) (pure '(1 2)))
      (pure '(3 4))))
   '((1 3) (2 4)))

  (check-equal?
   (call-with list-comonad
     (zapp*
      (trans (lambda-curried (x y z) (list x y z)) (pure '(1 2)))
      (pure '(3 4))
      (pure '(5 6))))
   '((1 3 5) (2 4 6)))

  (check-equal?
   (call-with list-comonad
     (zapp (trans (lambda-curried (x y) (list x y)) (pure '(1 2)))
	   (pure '(3 4))))
   '((1 3) (2 4)))

  (check-equal?
   (call-with list-comonad
     (extend (zapp (trans (lambda-curried (x y) (list x y)) (pure '(1 2)))
		   (pure '(3 4)))
	     extract))
   '((1 3) (2 4)))

  (check-equal?
   (call-with list-comonad
     (let-w ([x '(1 2)]
	     [y '(3 4)])
       (+ x y)))
   '(4 6))

  (check-equal?
   (call-with list-comonad
     (lift-w2 + '(1 2) '(3 4)))
   '(4 6))

  (check-equal?
   (call-with list-monad
     (lift-m2 + '(1 2) '(3 4)))
   '(4 5 5 6))

  (check-equal?
   (call-with list-monad
     (let-m ([a <- (return 3)]
	     [x <~ '(1 2)]
	     [y <~ '(3 4)])
       (return (+ (* a x) y))))
   '(6 7 9 10))) ; end of submodule test
