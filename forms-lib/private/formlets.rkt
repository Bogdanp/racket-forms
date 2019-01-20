#lang racket/base

(require racket/contract/base
         racket/string
         web-server/http
         "contracts.rkt"
         "unsafe/prim.rkt")

(provide
 (contract-out
  [ensure (->* (formlet/c) #:rest (listof formlet/c) formlet/c)]

  [required (->* () (#:message string?) formlet/c)]
  [matches (->* (regexp?) (#:message string?) formlet/c)]
  [one-of (->* ((listof (cons/c string? any/c))) (#:message string?) formlet/c)]
  [shorter-than (->* (exact-positive-integer?) (#:message string?) formlet/c)]
  [longer-than (->* (exact-positive-integer?) (#:message string?) formlet/c)]
  [to-boolean formlet/c]
  [to-number (->* () (#:message string?) formlet/c)]
  [to-symbol formlet/c]

  [binding/file formlet/c]
  [binding/text formlet/c]
  [binding/boolean formlet/c]
  [binding/email formlet/c]
  [binding/number formlet/c]
  [binding/symbol formlet/c]))

(define (ensure f . gs)
  (for/fold ([f f])
            ([g gs])
    (bind f g)))

(define ((lift f) v)
  (if v
      (f v)
      (ok v)))

(define ((required #:message [message "This field is required."]) v)
  (or (and v (not (string=? "" v)) (ok v))
      (err message)))

(define (matches p #:message [message (format "This field must match the regular expression ~v." p)])
  (lift (lambda (v)
          (if (regexp-match? p v)
              (ok v)
              (err message)))))

(define (one-of pairs #:message [message (format "This field must contain one of the following values: ~a" (string-join (map car pairs) ", "))])
  (lift (lambda (v)
          (define pair
            (findf (lambda (pair)
                     (string=? v (car pair))) pairs))
          (if pair
              (ok (cdr pair))
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

(define binding/file
  (lift (lambda (v)
          (if (binding:file? v)
              (ok v)
              (err "Expected a binding:file.")))))

(define binding/text
  (lift (lambda (v)
          (if (binding:form? v)
              (ok (bytes->string/utf-8 (binding:form-value v)))
              (err "Expected a binding:text.")))))

(define binding/boolean
  (ensure binding/text to-boolean))

(define binding/email
  (ensure binding/text (matches #rx".+@.+" #:message "This field must contain an e-mail address.")))

(define binding/number
  (ensure binding/text (to-number)))

(define binding/symbol
  (ensure binding/text to-symbol))
