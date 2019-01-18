#lang racket/base

(provide (all-defined-out))

(define (ok v)
  (cons 'ok v))

(define (ok? res)
  (and (pair? res) (eq? 'ok (car res))))

(define (err m)
  (cons 'err m))

(define (err? res)
  (and (pair? res) (eq? 'err (car res))))

(define ((alternate fa fb) v)
  (define res (fa v))
  (cond
    [(ok? res) res]
    [else (fb v)]))

(define ((bind fa fb) v)
  (define res (fa v))
  (cond
    [(ok? res) (fb (cdr res))]
    [else res]))
