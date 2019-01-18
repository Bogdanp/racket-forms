#lang racket/base

(require racket/contract
         racket/string
         web-server/http
         "contracts.rkt"
         "forms.rkt")

(provide widget-namespace
         widget-errors
         widget-input
         widget-email
         widget-text
         widget-password)

(define (lookup-errors errors full-name)
  (let loop ([path (map string->symbol (string-split full-name "."))]
             [errors errors])
    (define current (car path))
    (define remaining (cdr path))
    (define found? (assq current errors))

    (cond
      [found?
       (if (null? remaining)
           (cdr found?)
           (loop remaining (cdr found?)))]

      [else #f])))

(define/contract ((widget-namespace namespace widget-renderer) name widget)
  (-> string? widget-renderer/c widget-renderer/c)
  (widget-renderer (string-append namespace "." name) widget))

(define/contract ((widget-errors #:class [class "errors"]) name value errors)
  (->* () (#:class string?) widget/c)

  (define error-or-errors (lookup-errors errors name))
  (define (render error-messages)
    `((ul ((class ,class))
          ,@(map (lambda (error-message)
                   `(li ,error-message)) error-messages))))

  (cond
    [(string? error-or-errors)
     (render (list error-or-errors))]

    [(list? error-or-errors)
     (render (map (lambda (p) (string-append (car p) ": " (cdr p))) error-or-errors))]

    [else null]))

(define/contract ((widget-input #:type [type "text"]
                                #:class [class #f]
                                #:omit-value? [omit-value? #f]
                                #:required? [required? #f]
                                #:disabled? [disabled? #f]
                                #:min-length [min-length #f]
                                #:max-length [max-length #f]) name binding _)
  (->* () (#:type string?
           #:class (or/c false/c string?)
           #:omit-value? boolean?
           #:required? boolean?
           #:disabled? boolean?
           #:min-length (or/c false/c exact-positive-integer?)
           #:max-length (or/c false/c exact-positive-integer?)) widget/c)

  (define (optional attribute value)
    (cond
      [value (list (list attribute value))]
      [else null]))

  (define value
    (and (not omit-value?) binding (bytes->string/utf-8 (binding:form-value binding))))

  `(input ((type ,type)
           (name ,name)
           ,@(optional 'class class)
           ,@(optional 'required (and required? "required"))
           ,@(optional 'disabled (and disabled? "disabled"))
           ,@(optional 'minlength min-length)
           ,@(optional 'maxlength max-length)
           ,@(optional 'value value))))

(define/contract (widget-email #:class [class #f]
                               #:required? [required? #f]
                               #:disabled? [disabled? #f]
                               #:min-length [min-length #f]
                               #:max-length [max-length #f])
  (->* () (#:class (or/c false/c string?)
           #:required? boolean?
           #:disabled? boolean?
           #:min-length (or/c false/c exact-positive-integer?)
           #:max-length (or/c false/c exact-positive-integer?)) widget/c)

  (widget-input #:type "email"
                #:required? required?
                #:disabled? disabled?
                #:min-length min-length
                #:max-length max-length))

(define/contract (widget-text #:class [class #f]
                              #:required? [required? #f]
                              #:disabled? [disabled? #f]
                              #:min-length [min-length #f]
                              #:max-length [max-length #f])
  (->* () (#:class (or/c false/c string?)
           #:required? boolean?
           #:disabled? boolean?
           #:min-length (or/c false/c exact-positive-integer?)
           #:max-length (or/c false/c exact-positive-integer?)) widget/c)

  (widget-input #:required? required?
                #:disabled? disabled?
                #:min-length min-length
                #:max-length max-length))

(define/contract (widget-password #:class [class #f]
                                  #:required? [required? #f]
                                  #:disabled? [disabled? #f]
                                  #:min-length [min-length #f]
                                  #:max-length [max-length #f])
  (->* () (#:class (or/c false/c string?)
           #:required? boolean?
           #:disabled? boolean?
           #:min-length (or/c false/c exact-positive-integer?)
           #:max-length (or/c false/c exact-positive-integer?)) widget/c)

  (widget-input #:type "password"
                #:omit-value? #t
                #:required? required?
                #:disabled? disabled?
                #:min-length min-length
                #:max-length max-length))
