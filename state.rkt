#lang racket/base

(require racket/contract/base)

(provide
 (all-from-out "main.rkt")
 (contract-out
  [state-monad monad-state?])
 state-run state-run-with
 state-exec state-exec-with
 state-eval state-eval-with)

(module core racket/base
  (require racket/contract)
  
  (provide
   (contract-out   
    [run       run/c]
    [run-with  run-with/c]
    [exec      exec/c]
    [exec-with exec-with/c]
    [eval      exec/c]
    [eval-with exec-with/c]
    [trans     trans/c]
    [app       app/c]
    [return    return/c]
    [bind      bind/c]
    [join      join/c]   
    [get       get/c]
    [local     local/c]
    [put       put/c]))

  (require yutes)

  (struct stateful
    (proc)
    #:property prop:procedure (struct-field-index proc)
    #:transparent)

  (define run/c (-> stateful? any/c (values any/c any/c)))

  (define (run mx s)
    (mx s))

  (define run-with/c (-> any/c stateful?  (values any/c any/c)))
  
  (define (run-with s mx) (run mx s))

  (define exec/c (-> stateful? any/c any/c))
  (define (exec mx s)
    (let-values ([(x s) (run mx s)])
      x))

  (define exec-with/c (-> any/c stateful? any/c))
  
  (define (exec-with s mx)
    (exec mx s))

  (define (eval mx s)
    (let-values ([(x s) (run mx s)])
      s))

  (define (eval-with s mx)
    (eval mx s))

  (define-syntax define-stateful
    (syntax-rules ()
      [(_ (fun args ...) (state) expr)
       (define-curried (fun args ...)
	 (stateful
	  (lambda-curried (state)
	    expr)))]))

  (define stateful->stateful/c
    (case->
     (-> stateful? stateful?)
     (-> (recursive-contract stateful->stateful/c))))

  (define trans/c
    (case->
     (-> (-> any/c any/c) stateful? stateful?)
     (-> (-> any/c any/c) stateful->stateful/c)
     (-> (recursive-contract trans/c))))

  (define-stateful (trans f mx) (s)
    (let-values ([(x s) (run mx s)])
      (run (return (f x)) s)))


  (define app/c
    (case->
     (-> stateful? stateful? stateful?)
     (-> stateful? stateful->stateful/c)
     (-> (recursive-contract app/c))))
  
  (define-stateful (app mf mx) (s)
    (let*-values ([(f s) (run mf s)]
		  [(x s) (run mx s)])
      (run (return (f x)) s)))


  (define return/c
    (case->
     (-> any/c stateful?)
     (-> (recursive-contract return/c))))
  

  (define-stateful (return x) (s)	;
    (values x s))

  (define constructor->stateful/c
    (case->
     (-> (-> any/c stateful?) stateful?)
     (-> (recursive-contract constructor->stateful/c))))
  
  (define bind/c
    (case->
     (-> stateful? (-> any/c stateful?) stateful?)
     (-> stateful? (-> constructor->stateful/c))
     (-> (recursive-contract bind/c))))

 
  (define-stateful (bind mx f) (s)
    (let-values ([(x s) (run mx s)])
      (run (f x) s)))

  (define join/c
    (case->
     (-> stateful? stateful?)
     (-> (recursive-contract join/c))))

  (define-stateful (join mmx) (s)
    (let-values ([(mx s) (run mmx s)])
      (run mx s)))


  (define get/c (-> stateful?))

  (define-stateful (get) (s)
    (run (return s) s))

  (define local/c
    (case->
     (-> (-> any/c any/c) stateful? stateful?)
     (-> (-> any/c any/c) stateful->stateful/c)
     (-> (recursive-contract local/c))))

  (define-stateful (local f mx) (s)
    (bind (get)
	  (lambda (s)
	    (bind (put (f s))
		  (lambda (_)
		    mx)))))


  (define put/c
    (case->
     (-> any/c stateful?)
     (-> (recursive-contract put/c))))
  
  (define-stateful (put s) (_)
    (run (return '()) s))) ; end of submodule core

(require
 (prefix-in state- (submod "." core))
 "main.rkt")

(define state-monad
  (monad-state
   state-trans state-app state-return state-bind state-join
   state-get state-local state-put))



(module+ test
  (require yutes racket/match racket/contract rackunit)

  (check-equal? (get-trans state-monad) state-trans)
  (check-equal? (get-app state-monad) state-app)
  (check-equal? (get-return state-monad) state-return)
  (check-equal? (get-bind state-monad) state-bind)
  (check-equal? (get-join state-monad) state-join)
  (check-equal? (get-get state-monad) state-get)
  (check-equal? (get-local state-monad) state-local)
  (check-equal? (get-put state-monad) state-put)

  (contract-exercise
   state-trans state-app state-return state-bind state-join
   state-get state-local state-put)
  
  


  
  
  (define-syntax state-check-equal?
    (syntax-rules ()
      [(_ mexpr expected-value expected-state)
       (let-values ([(x s) mexpr])
	 (check-equal? x expected-value)
	 (check-equal? s expected-state))]))

  (define (twc x) (+ x x))

  (state-check-equal?
   (state-run-with 's (state-return 'x))
   'x 's)

  (state-check-equal?
   (state-run-with 's (state-trans twc (state-return 3)))
   6 's)

  (state-check-equal?
   (state-run
    (state-app (state-return twc) (state-return 3))
    's)
   6 's)

  (state-check-equal?
   (state-run
    (state-bind (state-return 3) state-return)
    's)
   3 's)

  (state-check-equal?
   (state-run  (state-put 'new-s) 's)
   '()
   'new-s)


  (state-check-equal?
   (state-run
    (call-with state-monad
      (get))
    's)
   's
   's)

  (state-check-equal?
   (state-run
    (call-with state-monad
      (begin-m
	  (put 'new-s)))
    'initial-s)
   '()
   'new-s)
  
  
  (define (update . input/state)
     (match input/state
       [`(0  (off . ,n)) `(on  . ,n)]
       [`(0  (on  . ,n)) `(off . ,n)]
       [`(1  (on  . ,n)) `(on  . ,(add1 n))]
       [`(-1 (on  . ,n)) `(on  . ,(sub1 n))]
       [(list _ state) state]))

  (define (game input)
    (match input
      [(list x xs ...)
       (let-m ([s <- (get)])
	 - (put (update x s))
	 (game xs))]
      ['() (get)]))

  (check-equal?
   (state-exec-with
    '(off . 0)
    (call-with state-monad
      (game '(0 1 0))))
   '(off . 1))) ; end of submodule test
