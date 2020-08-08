#lang racket/base

(require racket/contract/base
         web-server/http
         xml)

(provide (all-defined-out))

(define res/c (cons/c (or/c 'ok 'err) any/c))
(define attributes/c (listof (list/c symbol? string?)))
(define bindings/c (hash/c string? any/c))
(define errors/c (listof (or/c string? (cons/c symbol? (or/c string? (recursive-contract errors/c))))))
(define formlet/c (-> any/c res/c))
(define options/c (listof (cons/c string? string?)))
(define select-options/c
  (listof (or/c (cons/c string? string?)
                (list/c string? (listof (cons/c string? string?))))))
(define widget/c (-> string? (or/c false/c binding?) errors/c (or/c xexpr/c (listof xexpr/c))))
(define widget-renderer/c (-> string? widget/c (or/c xexpr/c (listof xexpr/c))))
(define validation/c (or/c (list/c 'passed any/c widget-renderer/c)
                           (list/c 'failed errors/c widget-renderer/c)
                           (list/c 'pending false/c widget-renderer/c)))
