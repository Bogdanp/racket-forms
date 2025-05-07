#lang racket/base

(require (for-syntax racket/base)
         racket/contract/base
         racket/format
         racket/function
         racket/lazy-require
         racket/list
         racket/match
         racket/string
         web-server/http
         (submod "contract.rkt" internal)
         "l10n.rkt"
         (submod "prim.rkt" unsafe))

(lazy-require
 [json (jsexpr? bytes->jsexpr)])

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
        (formlet-> (or/c #f string? bytes? path? input-port?)
                   (or/c #f string? bytes? path? input-port?)
                   #:err/c string?))]

  [one-of
   (->* [(listof (cons/c any/c any/c))]
        [#:message string?]
        (formlet-> any/c any/c #:err/c string?))]

  [shorter-than
   (->* [exact-positive-integer?]
        [#:message string?]
        (formlet-> (or/c #f string?)
                   (or/c #f string?)
                   #:err/c string?))]

  [longer-than
   (->* [exact-positive-integer?]
        [#:message string?]
        (formlet-> (or/c #f string?)
                   (or/c #f string?)
                   #:err/c string?))]

  [range/inclusive
   (->* [real? real?]
        [#:message string?]
        (formlet-> (or/c #f real?)
                   (or/c #f real?)
                   #:err/c string?))]

  [to-boolean
   (formlet-> any/c boolean?)]

  [to-number
   (->* []
        [#:message string?]
        (formlet-> (or/c #f string?)
                   (or/c #f number?)
                   #:err/c string?))]

  [to-integer
   (->* []
        [#:message string?]
        (formlet-> (or/c #f number?)
                   (or/c #f integer?)
                   #:err/c string?))]

  [to-real
   (->* []
        [#:message string?]
        (formlet-> (or/c #f number?)
                   (or/c #f real?)
                   #:err/c string?))]

  [to-symbol
   (formlet-> (or/c #f string?)
              (or/c #f symbol?))]

  [list-of
   (-> (formlet-> (or/c #f binding?)
                  any/c
                  #:err/c string?)
       (formlet-> (or/c #f (listof (or/c #f string?)))
                  (listof (or/c #f any/c))
                  #:err/c (listof string?)))]

  [list-of*
   (->* []
        [#:too-few-elements-message string?
         #:too-many-elements-message string?]
        #:rest (listof
                (formlet-> (or/c #f binding?)
                           any/c
                           #:err/c string?))
        (formlet-> (or/c #f (listof (or/c #f string?)))
                   (listof (or/c #f any/c))
                   #:err/c (listof string?)))]

  [list-of-length
   (->* [exact-nonnegative-integer?]
        [#:too-few-elements-message string?
         #:too-many-elements-message string?]
        (formlet-> (listof (or/c #f string?))
                   (listof (or/c #f string?))
                   #:err/c (listof string?)))]

  [binding/json
   (formlet-> (or/c #f binding?)
              (or/c #f jsexpr?)
              #:err/c string?)]

  [binding/file
   (formlet-> (or/c #f binding?)
              (or/c #f binding:file?)
              #:err/c string?)]

  [binding/text
   (formlet-> (or/c #f binding?)
              (or/c #f string?)
              #:err/c string?)]

  [binding/boolean
   (formlet-> (or/c #f binding?)
              boolean?
              #:err/c string?)]

  [binding/email
   (formlet-> (or/c #f binding?)
              (or/c #f string?)
              #:err/c string?)]

  [binding/number
   (formlet-> (or/c #f binding?)
              (or/c #f number?)
              #:err/c string?)]

  [binding/symbol
   (formlet-> (or/c #f binding?)
              (or/c #f symbol?)
              #:err/c string?)]

  [binding/list
   (formlet-> (or/c #f binding? (listof binding?))
              (or/c #f (listof (or/c #f string?)))
              #:err/c string?)]))

(define (ensure f . gs)
  (for/fold ([f f])
            ([g gs])
    (bind f g)))

(define (lift who f)
  (case-lambda
    [() (raise-arity-error who 1)]
    [(v) (if v (f v) (ok v))]
    [args (apply raise-arity-error who 1 args)]))

(define ((required #:message [message #f]) v)
  (if v
      (ok v)
      (err (or message (translate 'err-required)))))

(define (matches p #:message [message #f])
  (lift
   'matches
   (lambda (v)
     (if (regexp-match? p v)
         (ok v)
         (err (or message (translate 'err-matches p)))))))

(define (one-of pairs #:message [message #f])
  (lift
   'one-of
   (lambda (v)
     (cond
       [(findf (lambda (pair)
                 (equal? v (car pair))) pairs)
        => (compose1 ok cdr)]

       [else (err (or message (translate 'err-one-of (string-join (map (compose1 (curry format "~a") car) pairs) ", "))))]))))

(define (shorter-than n #:message [message #f])
  (lift
   'shorter-than
   (lambda (v)
     (if (< (string-length v) n)
         (ok v)
         (err (or message (translate 'err-shorter-than (sub1 n))))))))

(define (longer-than n #:message [message #f])
  (lift
   'longer-than
   (lambda (v)
     (if (> (string-length v) n)
         (ok v)
         (err (or message (translate 'err-longer-than (add1 n))))))))

(define (range/inclusive min max #:message [message #f])
  (lift
   'range/inclusive
   (lambda (v)
     (if (<= min v max)
         (ok v)
         (err (or message (translate 'err-range/inclusive (~r min) (~r max))))))))

(define (to-boolean v)
  (ok (not (not v))))

(define (to-number #:message [message #f])
  (lift
   'to-number
   (lambda (v)
     (cond
       [(string->number v) => ok]
       [else (err (or message (translate 'err-to-number)))]))))

(define (to-integer #:message [message #f])
  (lift
   'to-integer
   (lambda (v)
     (cond
       [(integer? v)
        (if (inexact? v)
            (ok (inexact->exact v))
            (ok v))]
       [else (err (or message (translate 'err-to-integer)))]))))

(define (to-real #:message [message #f])
  (lift
   'to-real
   (lambda (v)
     (cond
       [(real? v) (ok v)]
       [else (err (or message (translate 'err-to-real)))]))))

(define (to-symbol v)
  (ok (and v (string->symbol v))))

(define ((list-of formlet) lst)
  (define res
    (for/fold ([res (ok null)])
              ([v (in-list (or lst null))])
      (match (formlet (binding:form #"" (if v (string->bytes/utf-8 v) #"")))
        [`(ok . ,v)
         (bimap
          (λ (vs) (cons v vs))
          (λ (errs) (cons "" errs))
          res)]
        [`(err . ,e)
         (bind2
          (λ (vs) (err (cons e (make-list (length vs) ""))))
          (λ (errs) (err (cons e errs)))
          res)])))
  (bimap reverse reverse res))

(define ((list-of*
          #:too-few-elements-message [too-few-message (translate 'err-list-elt-required)]
          #:too-many-elements-message [too-many-message (translate 'err-list-elt-overflow)]
          . formlets) lst)
  (let ([lst (or lst null)])
    (define n (length formlets))
    (define m (length lst))
    (define res
      (for/fold ([res (ok null)])
                ([formlet (in-list formlets)]
                 [v (in-list lst)])
        (match (formlet (binding:form #"" (if v (string->bytes/utf-8 v) #"")))
          [`(ok . ,v)
           (bimap
            (λ (vs) (cons v vs))
            (λ (errs) (cons "" errs))
            res)]
          [`(err . ,e)
           (bind2
            (λ (vs) (err (cons e (make-list (length vs) ""))))
            (λ (errs) (err (cons e errs)))
            res)])))
    (cond
      [(n . = . m)
       (bimap reverse reverse res)]
      [(n . < . m)
       (bind2
        (λ (_) (err (make-list n too-many-message)))
        (λ (errs) (err (append errs (make-list (m . - . n) too-many-message))))
        res)]
      [else
       (bind2
        (λ (_) (err (append (make-list m "") (make-list (n . - . m) too-few-message))))
        (λ (errs) (err (append errs (make-list (n . - . m) too-few-message))))
        res)])))

(define ((list-of-length
          #:too-few-elements-message [too-few-message (translate 'err-list-elt-required)]
          #:too-many-elements-message [too-many-message (translate 'err-list-elt-overflow)]
          n) lst)
  (cond
    [((length lst) . = . n)
     (ok lst)]
    [((length lst) . > . n)
     (err
      (append
       (make-list n "")
       (make-list ((length lst) . - . n) too-many-message)))]
    [else
     (err
      (append
       (make-list (length lst) "")
       (make-list (n . - . (length lst)) too-few-message)))]))

(define binding/file
  (lift
   'binding/file
   (lambda (v)
     (if (and (binding:file? v))
         (if (bytes=? (binding:file-filename v) #"")
             (ok #f)
             (ok v))
         (err "Expected a binding:file.")))))

(define binding/text
  (lift
   'binding/text
   (match-lambda
     [(binding-string-value v)
      (ok v)]
     [_
      (err "Expected a binding:form.")])))

(define binding/json
  (lift
   'binding/json
   (match-lambda
     [(binding-bytes-value v)
      (with-handlers ([exn:fail? (λ (_) (err "Expected a valid JSON value."))])
        (ok (bytes->jsexpr v)))]
     [_
      (err "Expected a valid JSON value.")])))

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

(define binding/list
  (lift
   'binding/list
   (match-lambda
     [(? list? xs)
      (let loop ([xs xs]
                 [ys null])
        (match xs
          ['()
           (ok (reverse ys))]
          [`(,(binding-string-value v) . ,xs)
           (loop xs (cons v ys))]
          [_
           (err "Expected a list of binding:form values.")]))]
     [(binding-string-value v)
      (ok (list v))]
     [_
      (err "Expected a list of binding:form values.")])))

(define-match-expander binding-bytes-value
  (lambda (stx)
    (syntax-case stx ()
      [(_ v)
       #'(binding:form _ v)])))

(define-match-expander binding-string-value
  (lambda (stx)
    (syntax-case stx ()
      [(_ v)
       #'(binding-bytes-value (app parse-bytes v))])))

(define (parse-bytes bs)
  (let ([s (bytes->string/utf-8 bs)])
    (and (non-empty-string? s) s)))
