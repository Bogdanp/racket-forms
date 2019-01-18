#lang racket/base

(require racket/contract/base
         "contracts.rkt"
         "unsafe/prim.rkt")

(provide
 (contract-out
  [ok (-> any/c res/c)]
  [ok? (-> any/c boolean?)]
  [err (-> any/c res/c)]
  [err? (-> any/c boolean?)]
  [alternate (-> formlet/c formlet/c formlet/c)]
  [bind (-> formlet/c formlet/c formlet/c)]))
