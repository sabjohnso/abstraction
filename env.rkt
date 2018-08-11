#lang racket/base

(require racket/contract)

(provide
 env-run
 (contract-out
  [env-monad monad-environment?])
 (all-from-out "main.rkt"))


(module core racket/base
  (require racket/contract)

  (provide
   (contract-out
    [trans  trans/c]
    [app    app/c]
    [return return/c]
    [bind   bind/c]
    [join   join/c]
    [get    get/c]
    [local  local/c]
    [run    run/c]))
  
  (require yutes)
  
  (struct environment
    (proc)
    #:property prop:procedure (struct-field-index proc)
    #:transparent)

  (define-syntax define-env
    (syntax-rules ()
      [(_ (fun args ...) (env) expr)
       (define-curried (fun args ...)
	 (environment
	  (lambda-curried (env) expr)))]))

  (define environment->environment/c
    (case-> (-> environment? environment?)
	    (-> (recursive-contract environment->environment/c))))

  (define trans/c
    (case->
     (-> (-> any/c any/c) environment? environment?)
     (-> (-> any/c any/c) environment->environment/c)
     (-> (recursive-contract trans/c))))
  
  (define-env (trans f mx) (env)
    (f (mx env)))


  (define app/c
    (case->
     (-> environment? environment? environment?)
     (-> environment? environment->environment/c)
     (-> (recursive-contract app/c))))
  
  (define-env (app mf mx) (env)
    ((mf env) (mx env)))

  (define return/c
    (case->
     (-> any/c environment?)
     (-> (recursive-contract return/c))))
  
  (define-env (return x) (env) x)

  (define constructor->environment/c
    (case->
     (-> (-> any/c environment?) environment?)
     (-> (recursive-contract constructor->environment/c))))

  (define bind/c
    (case->
     (-> environment? (-> any/c  environment?) environment?)
     (-> environment?  constructor->environment/c)
     (-> (recursive-contract bind/c))))
  
  (define-env (bind mx f) (env)
    ((f (mx env)) env))

  
  (define join/c		       
    (case->			
     (-> environment? environment?)
     (-> (recursive-contract join/c))))
  
  (define-env (join mmx) (env)
    ((mmx env) env))

  (define get/c
    (-> environment?))
  

  (define-env (get) (env)
    env)

  (define local/c
    (case->
     (-> (-> any/c any/c) environment? environment?)
     (-> (recursive-contract local/c))))

  (define-env (local f mx) (env)
    (mx (f env)))

  (define run/c (-> environment? any/c any/c))
  
  (define (run mx e)
    (mx e))
  ) ; end of submodule core


(require
 (prefix-in env- (submod "." core))
 "main.rkt")

(define env-monad
  (monad-environment env-trans env-app env-return env-bind env-join env-get env-local))


(module+ test
  (require yutes rackunit)

  (contract-exercise env-trans)
  (contract-exercise env-app)
  (contract-exercise env-return)
  (contract-exercise env-bind)
  (contract-exercise env-join)
  (contract-exercise env-get)
  (contract-exercise env-local)
  (contract-exercise env-run)
  
  
  (check-equal? (get-trans env-monad) env-trans)
  (check-equal? (get-app env-monad) env-app)
  (check-equal? (get-return env-monad) env-return)
  (check-equal? (get-bind env-monad) env-bind)
  (check-equal? (get-join env-monad) env-join)
  (check-equal? (get-get env-monad) env-get)
  (check-equal? (get-local env-monad) env-local)

  (define (sqr x) (* x x))
  (define (twc x) (+ x x))
  (define (add x y) (+ x y))

  (check-equal?
   (env-run (env-trans sqr (env-return 3)) 'e)
   9)

  (check-equal?
   (env-run (env-get) 'e)
   'e)

  (check-equal?
   (env-run (env-local (lambda (_) 'e2) (env-get)) 'e1)
   'e2)

  (check-equal?
   (env-run (env-bind (env-return 'x) env-return) 'e)
   'x)

  (check-equal?
   (env-run (env-bind (env-get) env-return) 'e)
   'e)

  (check-equal?
   (env-run (env-join (env-return (env-get))) 'e)
   'e)

  (check-equal?
   (env-run
    (call-with env-monad
      (let-m ([e <- (get)])
	(return e)))
    'e)
   'e)

  (check-equal?
   (env-run
    (call-with env-monad
      (let-m ([e1 <- (get)]
	      [e2 <- (local (lambda (_) 'e2) (get))])
	(return (cons e1 e2))))
    'e1)
   '(e1 . e2))

  
  
  
  ) ; end of submodule test


