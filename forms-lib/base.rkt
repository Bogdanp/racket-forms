#lang racket/base

(define-syntax-rule (provide-all-from m ...)
  (begin
    (require m ...)
    (provide (all-from-out m ...))))

(provide-all-from "formlets.rkt"
                  "forms.rkt"
                  "widgets.rkt")
