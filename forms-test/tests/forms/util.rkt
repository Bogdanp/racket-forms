#lang racket/base

(require web-server/http)

(provide (all-defined-out))

(define (make-binding s #:name [name #"fake-binding"])
  (binding:form name (string->bytes/utf-8 s)))
