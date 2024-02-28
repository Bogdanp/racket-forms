#lang racket/base

(define-syntax-rule (reprovide m ...)
  (begin
    (require m ...)
    (provide (all-from-out m ...))))

(reprovide
 "private/contract.rkt"
 "private/formlet.rkt"
 "private/form.rkt"
 "private/prim.rkt"
 "private/widget.rkt")
