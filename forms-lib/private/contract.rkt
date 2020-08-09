#lang racket/base

(require racket/contract
         web-server/http
         (only-in xml xexpr/c))

(module internal racket/base
  (require racket/contract)
  (provide (all-defined-out))

  (define res/c (cons/c (or/c 'ok 'err) any/c))
  (define bindings/c (hash/c string? any/c))
  (define formlet/c (-> any/c res/c))

  (define (formlet-> in/c
                     ok-res/c
                     #:err/c [err-res/c the-unsupplied-arg])
    (-> in/c (if (unsupplied-arg? err-res/c)
                 (cons/c 'ok ok-res/c)
                 (or/c (cons/c 'ok ok-res/c)
                       (cons/c 'err err-res/c))))))

(require 'internal)
(provide (all-defined-out))

(define attributes/c
  (listof (list/c symbol? string?)))

(define errors/c
  (listof (or/c string? (cons/c symbol? (or/c string? (recursive-contract errors/c))))))

(define radio-options/c
  (listof (cons/c string? string?)))

(define select-options/c
  (listof (or/c (cons/c string? string?)
                (list/c string? (listof (cons/c string? string?))))))

(define widget/c
  (-> string? (or/c #f binding?) errors/c (or/c xexpr/c (listof xexpr/c))))

(define widget-renderer/c
  (-> string? widget/c (or/c xexpr/c (listof xexpr/c))))

(define validation-result/c
  (or/c (list/c 'passed any/c widget-renderer/c)
        (list/c 'failed errors/c widget-renderer/c)
        (list/c 'pending #f widget-renderer/c)))
