#lang racket/base

(require racket/contract/base
         web-server/http
         "contracts.rkt"
         "unsafe/prim.rkt")

(provide
 (contract-out
  [ensure (->* (formlet/c) #:rest (listof formlet/c) formlet/c)]

  [default (-> any/c formlet/c)]
  [required (->* () (#:message string?) formlet/c)]
  [matches (->* (regexp?) (#:message string?) formlet/c)]
  [shorter-than (->* (exact-positive-integer?) (#:message string?) formlet/c)]
  [longer-than (->* (exact-positive-integer?) (#:message string?) formlet/c)]
  [to-boolean formlet/c]
  [to-number (->* () (#:message string?) formlet/c)]
  [to-symbol formlet/c]

  [binding/text formlet/c]
  [field/text formlet/c]
  [text formlet/c]
  [email formlet/c]))

(define (ensure f . gs)
  (for/fold ([f f])
            ([g gs])
    (bind f g)))

(define ((lift f) v)
  (if v
      (f v)
      (ok v)))

(define ((default x) v)
  (if (not v)
      (ok x)
      (ok v)))

(define ((required #:message [message "This field is required."]) v)
  (or (and v (ok v))
      (err message)))

(define (matches p #:message [message (format "This field must match the regular expression ~v." p)])
  (lift (lambda (v)
          (if (regexp-match? p v)
              (ok v)
              (err message)))))

(define (shorter-than n #:message [message (format "This field must contain ~a or fewer characters." (sub1 n))])
  (lift (lambda (v)
          (if (< (string-length v) n)
              (ok v)
              (err message)))))

(define (longer-than n #:message [message (format "This field must contain ~a or more characters." (add1 n))])
  (lift (lambda (v)
          (if (> (string-length v) n)
              (ok v)
              (err message)))))

(define (to-boolean v)
  (ok (and v #t)))

(define (to-number #:message [message (format "This field must contain a number.")])
  (lift (lambda (v)
          (define n (string->number v))
          (if n
              (ok n)
              (err message)))))

(define (to-symbol v)
  (ok (and v (string->symbol v))))

(define binding/text
  (lift (lambda (v)
          (if (binding:form? v)
              (ok (bytes->string/utf-8 (binding:form-value v)))
              (err "Expected a binding:form field.")))))

(define field/text
  (lift (lambda (v)
          (if (string? v)
              (ok v)
              (err "Expected a text field.")))))

(define text
  (alternate binding/text field/text))

(define email
  (ensure text (matches #rx".+@.+")))
