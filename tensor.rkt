#lang racket/base

(module core racket/base
  (require racket/contract/base yutes)
  (provide  (contract-out))

  (struct tensor
    (proc shape position)
    #:transparent
    #:property prop:procedure
    (lambda-curried (this indices rec)
      ((tensor-proc this) indices rec)))) ; end of submodule core

(require (submod "." core))

(module+ test
  (require rackunit)) ; end of submodule test










