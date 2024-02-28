#lang racket/base

(require racket/contract/base
         racket/format
         racket/function
         racket/match
         racket/string
         web-server/http
         "l10n.rkt"
         (submod "contract.rkt" internal)
         (submod "prim.rkt" unsafe))

(provide
 (contract-out
  [ensure
   (-> formlet/c formlet/c ... formlet/c)]

  [required
   (->* []
        [#:message string?]
        (formlet-> any/c any/c #:err/c string?))]

  [matches
   (->* [regexp?]
        [#:message string?]
        (formlet-> (or/c string? bytes? path? input-port? #f)
                   (or/c string? bytes? path? input-port? #f)
                   #:err/c string?))]

  [one-of
   (->* [(listof (cons/c any/c any/c))]
        [#:message string?]
        (formlet-> any/c any/c #:err/c string?))]

  [shorter-than
   (->* [exact-positive-integer?]
        [#:message string?]
        (formlet-> (or/c string? #f)
                   (or/c string? #f)
                   #:err/c string?))]

  [longer-than
   (->* [exact-positive-integer?]
        [#:message string?]
        (formlet-> (or/c string? #f)
                   (or/c string? #f)
                   #:err/c string?))]

  [range/inclusive
   (->* [real? real?]
        [#:message string?]
        (formlet-> (or/c real? #f)
                   (or/c real? #f)
                   #:err/c string?))]

  [to-boolean
   (formlet-> any/c boolean?)]

  [to-number
   (->* []
        [#:message string?]
        (formlet-> (or/c string? #f)
                   (or/c number? #f)
                   #:err/c string?))]

  [to-real
   (->* []
        [#:message string?]
        (formlet-> (or/c number? #f)
                   (or/c real? #f)
                   #:err/c string?))]

  [to-symbol
   (formlet-> (or/c string? #f)
              (or/c symbol? #f))]

  [binding/file
   (formlet-> (or/c binding? #f)
              (or/c binding:file? #f)
              #:err/c string?)]

  [binding/text
   (formlet-> (or/c binding? #f)
              (or/c string? #f)
              #:err/c string?)]

  [binding/boolean
   (formlet-> (or/c binding? #f)
              boolean?
              #:err/c string?)]

  [binding/email
   (formlet-> (or/c binding? #f)
              (or/c string? #f)
              #:err/c string?)]

  [binding/number
   (formlet-> (or/c binding? #f)
              (or/c number? #f)
              #:err/c string?)]

  [binding/symbol
   (formlet-> (or/c binding? #f)
              (or/c symbol? #f)
              #:err/c string?)]))

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

(define (range/inclusive min max #:message [message #f])
  (lift (lambda (v)
          (if (<= min v max)
              (ok v)
              (err (or message (translate 'err-range/inclusive (~r min) (~r max))))))))

(define (to-boolean v)
  (ok (not (not v))))

(define (to-number #:message [message #f])
  (lift (lambda (v)
          (cond
            [(string->number v) => ok]
            [else (err (or message (translate 'err-to-number)))]))))

(define (to-real #:message [message #f])
  (lift (lambda (v)
          (cond
            [(real? v) (ok v)]
            [else (err (or message (translate 'err-to-real)))]))))

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
  (lift (match-lambda
          [(binding:form _ (app bytes->string/utf-8 v))
           (ok (and (non-empty-string? v) v))]

          [_
           (err "Expected a binding:form.")])))

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
