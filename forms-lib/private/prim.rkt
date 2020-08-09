#lang racket/base

(require racket/contract/base
         (submod "contract.rkt" internal))

(module unsafe racket/base
  (provide (all-defined-out))

  (define (ok v)
    (cons 'ok v))

  (define (ok? res)
    (and (pair? res) (eq? 'ok (car res))))

  (define (err m)
    (cons 'err m))

  (define (err? res)
    (and (pair? res) (eq? 'err (car res))))

  (define ((bind fa fb) v)
    (define res (fa v))
    (cond
      [(ok? res) (fb (cdr res))]
      [else res])))

(require 'unsafe)
(provide
 (contract-out
  [ok (-> any/c res/c)]
  [ok? (-> any/c boolean?)]
  [err (-> any/c res/c)]
  [err? (-> any/c boolean?)]
  [bind (-> formlet/c formlet/c formlet/c)]))
