#lang racket/base


(provide
 (all-defined-out)
 (all-from-out "environment.rkt"))

(require
 (only-in yutes lambda-curried call-with cut)
 "environment.rkt")

(define-syntax begin-m
  (syntax-rules (- ~)
    [(_ e) e]
    [(_ - e1 e2 es ...)
     (bind e1
	   (lambda (_)
	     (begin-m e2 es ...)))]
    [(_ ~ e1 e2 es ...)
     (bind (pure e1)
	   (lambda (_)
	     (begin-m e2 es ...)))]
    [(_ e1 e2 es ...)
     (bind (pure e1)
	   (lambda (_)
	     (begin-m e2 es ...)))]))



(define-syntax let-m
  (syntax-rules (<~ <- <=)
    [(_ ([x <- mx]) e es ...) (bind mx (lambda (x) (begin-m e es ...)))]
    [(_ ([x <~ mx]) e es ...) (bind (pure mx) (lambda (x) (begin-m e es ...)))]
    [(_ ([x mx]) e es ...)    (bind (pure mx) (lambda (x) (begin-m e es ...)))]
    [(_ ([x <= y]) e es ...)  (bind (return y) (lambda (x) (begin-m e es ...)))]
    [(_ ([x <- mx] binding bindings ...) e es ...)
     (bind mx
	   (lambda (x)
	     (let-m (binding bindings ...)
	       e es ...)))]
    [(_ ([x <~ mx] binding bindings ...) e es ...)
     (bind (pure mx)
	   (lambda (x)
	     (let-m (binding bindings ...)
	       e es ...)))]
    
    [(_ ([x mx] binding bindings ...) e es ...)
     (bind (pure mx)
	   (lambda (x)
	     (let-m (binding bindings ...)
	       e es ...)))]
    [(_ ([x <= y] binding bindings ...) e es ...)
     (bind (return y)
	   (lambda (x)
	     (let-m (binding bindings ...)
	       e es ...)))]))

(define-syntax let-w
  (syntax-rules ()
    [(_ ([x wx]) e)
     (extend (pure wx)
	     (lambda (arg)
	       (lambda (w)
		 (let ([x (call-with w (extract arg))])
		   e))))]
    [(_ ([x wx] [y wy] [zs wzs] ...) e)
     (extend
      (zapp* (trans (lambda-curried (x y zs ...) (list x y zs ...)) (pure wx))
	     (pure wy)
	     (pure wzs) ...)
      (lambda (arg)
	(lambda (w)
	  (let-values ([(x y zs ...) (apply values (call-with w (extract arg)))])
	    e))))]
    
    #;
    [(_ ([x wx] [y wy] [zs wzs] ...) e)	;
    (trans				;
    (compose (lambda (x y zs ...) e)	;
    (lambda (arg) (apply values arg)))	;
    (zapp* (trans (lambda-curried (x y zs ...) (list x y zs ...)) (pure wx)) ;
    (pure wy)				;
    (pure wzs) ...))]))



