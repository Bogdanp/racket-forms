#lang racket/base

(require racket/contract/base
         (submod "contract.rkt" internal))

(module unsafe racket/base
  (require racket/match)
  (provide (all-defined-out))

  (define (ok v)
    (cons 'ok v))

  (define (ok? res)
    (and (pair? res) (eq? 'ok (car res))))

  (define (err m)
    (cons 'err m))

  (define (err? res)
    (and (pair? res) (eq? 'err (car res))))

  (define (bimap fa fb v)
    (bind2
     (compose1 ok fa)
     (compose1 err fb)
     v))

  (define ((bind fa fb) v)
    (define res (fa v))
    (cond
      [(ok? res) (fb (cdr res))]
      [else res]))

  (define (bind2 fok ferr v)
    (match v
      [`(ok . ,V) (fok V)]
      [`(err . ,V) (ferr V)])))

(require 'unsafe)
(provide
 (contract-out
  [ok (-> any/c res/c)]
  [ok? (-> any/c boolean?)]
  [err (-> any/c res/c)]
  [err? (-> any/c boolean?)]
  [bind (-> formlet/c formlet/c formlet/c)]))
