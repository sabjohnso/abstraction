#lang racket/base

(module core racket/base
  (require racket/contract/base racket/vector yutes "tools.rkt")

  (provide
   (struct-out pointer)
   vector->pointer
   pointer->vector
   build-pointer
   next
   prev
   move-by
   slice
   deref
   trans
   zapp
   extract
   duplicate
   extend)

  (struct pointer (data start size offset) #:transparent)
  (define-curried (vector->pointer data)
    (pointer data 0 (vector-length data) 0))

  (define-curried (pointer->vector ptr)
    (let ([data (pointer-data ptr)])
      (for/vector ([offset (pointer-size ptr)])
	(extract (move-by ptr offset)))))

  (define-curried (build-pointer n proc)
    (vector->pointer (build-vector n proc)))

  
  

  (define-curried (next ptr)
    (struct-copy pointer ptr [offset (add1 (pointer-offset ptr))]))

  (define-curried (prev ptr)
    (struct-copy pointer ptr [offset (sub1 (pointer-offset ptr))]))

  (define-curried (move-by ptr additional-offset)
    (struct-copy pointer ptr [offset (+ (pointer-offset ptr) additional-offset)]))

  (define-curried (deref ptr)
    (vector-ref (pointer-data ptr)
		(+ (pointer-start ptr)
		   (modulo (pointer-offset ptr)
			   (pointer-size ptr)))))

  (define-curried (slice ptr start size)
    (struct-copy pointer ptr [start (+ start (pointer-start ptr))] [size size]))

  (define-curried (trans f ptr)
    (struct-copy pointer ptr [data (vector-map f (pointer-data ptr))]))
  
  (define-curried (extract ptr)
    (deref ptr))

  (define-curried (duplicate ptr)
    (let ([n (pointer-size ptr)])
      (vector->pointer
       (for/vector ([offset (in-range n)])
	 (move-by ptr offset)))))

  (define-curried (zapp ptr-f ptr-x)
    (let ([n (pointer-size ptr-f)])
      (vector->pointer
       (for/vector ([offset (in-range n)])
	 ((extract (move-by ptr-f offset))
	  (extract (move-by ptr-x offset)))))))
  
  (define-curried (extend wx f)
    (trans f (duplicate wx)))); end of submodule core


(require
 (except-in (submod "." core) trans zapp extract extend duplicate)
 (prefix-in pointer- (only-in (submod "." core) trans zapp extract extend duplicate))
 "main.rkt")

(define pointer-comonad
  (comonad pointer-trans pointer-zapp pointer-extract pointer-extend pointer-duplicate))

(module+ test
  (require yutes (only-in racket/function identity)
	   racket/pretty rackunit)
  
  (define (twc x)
    (+ x x))

  (check-equal?
   (vector->pointer (build-vector 10 (lambda (index) index)))
   (pointer '#(0 1 2 3 4 5 6 7 8 9) 0 10 0))

  (check-equal?
   (pointer-trans twc (vector->pointer (build-vector 10 (lambda (index) index))))
   (vector->pointer
    (build-vector 10
      (lambda (index) (twc index)))))
  
  (check-equal?
   (pointer-zapp
    (vector->pointer (make-vector 10 twc))
    (vector->pointer (build-vector 10 identity)))
   (pointer '#(0 2 4 6 8 10 12 14 16 18) 0 10 0))

  (check-equal?
   (pointer-extend (vector->pointer (build-vector 10 identity)) pointer-extract)
   (vector->pointer (build-vector 10 identity)))

  (check-equal?
   (pointer-duplicate (vector->pointer (build-vector 10 identity)))
   (pointer
    (vector
     (pointer '#(0 1 2 3 4 5 6 7 8 9) 0 10 0)
     (pointer '#(0 1 2 3 4 5 6 7 8 9) 0 10 1)
     (pointer '#(0 1 2 3 4 5 6 7 8 9) 0 10 2)
     (pointer '#(0 1 2 3 4 5 6 7 8 9) 0 10 3)
     (pointer '#(0 1 2 3 4 5 6 7 8 9) 0 10 4)
     (pointer '#(0 1 2 3 4 5 6 7 8 9) 0 10 5)
     (pointer '#(0 1 2 3 4 5 6 7 8 9) 0 10 6)
     (pointer '#(0 1 2 3 4 5 6 7 8 9) 0 10 7)
     (pointer '#(0 1 2 3 4 5 6 7 8 9) 0 10 8)
     (pointer '#(0 1 2 3 4 5 6 7 8 9) 0 10 9))
    0 10 0))

  (let ([xs (build-pointer 10 identity)])

    (check-equal?
     (call-with pointer-comonad
       (let-w ([xm (prev xs)]
	       [xi xs]
	       [xp (next xs)])
	 (+ xm (* 2 xi) xp)))
     (pointer '#(10 4 8 12 16 20 24 28 32 26) 0 10 0))
    
    (check-equal?
     (pointer->vector (slice xs 2 3))
     #(2 3 4)))) ; end of submodule test
