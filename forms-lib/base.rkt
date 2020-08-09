#lang racket/base

(define-syntax-rule (provide-all-from m ...)
  (begin
    (require m ...)
    (provide (all-from-out m ...))))

(provide-all-from "private/formlet.rkt"
                  "private/form.rkt"
                  "private/widget.rkt")
