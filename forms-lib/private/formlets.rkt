#lang racket/base

(require racket/contract/base
         racket/function
         racket/string
         web-server/http
         "contracts.rkt"
         "l10n.rkt"
         "unsafe/prim.rkt")

(provide
 (contract-out
  [ensure (->* (formlet/c) #:rest (listof formlet/c) formlet/c)]

  [required (->* () (#:message string?) formlet/c)]
  [matches (->* (regexp?) (#:message string?) formlet/c)]
  [one-of (->* ((listof (cons/c any/c any/c))) (#:message string?) formlet/c)]
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

(define ((required #:message [message #f]) v)
  (if v
      (ok v)
      (err (or message (translate 'err-required)))))

(define (matches p #:message [message #f])
  (lift (lambda (v)
          (if (regexp-match? p v)
              (ok v)
              (err (or message (translate 'err-matches p)))))))

(define (one-of pairs #:message [message #f])
  (lift (lambda (v)
          (cond
            [(findf (lambda (pair)
                      (equal? v (car pair))) pairs)
             => (compose1 ok cdr)]

            [else (err (or message (translate 'err-one-of (string-join (map (compose1 (curry format "~a") car) pairs) ", "))))]))))

(define (shorter-than n #:message [message #f])
  (lift (lambda (v)
          (if (< (string-length v) n)
              (ok v)
              (err (or message (translate 'err-shorter-than (sub1 n))))))))

(define (longer-than n #:message [message #f])
  (lift (lambda (v)
          (if (> (string-length v) n)
              (ok v)
              (err (or message (translate 'err-longer-than (add1 n))))))))

(define (to-boolean v)
  (ok (not (not v))))

(define (to-number #:message [message #f])
  (lift (lambda (v)
          (define n (string->number v))
          (if n
              (ok n)
              (err (or message (translate 'err-to-number)))))))

(define (to-symbol v)
  (ok (and v (string->symbol v))))

(define binding/file
  (lift (lambda (v)
          (if (and (binding:file? v))
              (if (bytes=? (binding:file-filename v) #"")
                  (ok #f)
                  (ok v))
              (err "Expected a binding:file.")))))

(define binding/text
  (lift (lambda (v)
          (if (binding:form? v)
              (let ([v (bytes->string/utf-8 (binding:form-value v))])
                (if (non-empty-string? v)
                    (ok v)
                    (ok #f)))
              (err "Expected a binding:form.")))))

(define binding/boolean
  (ensure binding/text to-boolean))

(define binding/email
  (ensure binding/text
          (lambda (v)
            ;; delay execution s.t. the translation is done as late as possible.
            ((matches #rx".+@.+" #:message (translate 'err-binding/email)) v))))

(define binding/number
  (ensure binding/text (to-number)))

(define binding/symbol
  (ensure binding/text to-symbol))
