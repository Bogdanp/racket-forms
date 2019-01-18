#lang racket/base

(require racket/contract/base
         racket/string
         web-server/http
         "contracts.rkt"
         "forms.rkt")

(provide
 (contract-out
  [widget-namespace (-> string? widget-renderer/c widget-renderer/c)]
  [widget-errors (->* () (#:class string?) widget/c)]
  [widget-input (->* () (#:type string?
                         #:omit-value? boolean?
                         #:attributes attributes/c) widget/c)]
  [widget-textarea (->* () (#:omit-value? boolean?
                            #:attributes attributes/c) widget/c)]
  [widget-email (->* () (#:attributes attributes/c) widget/c)]
  [widget-file (->* () (#:attributes attributes/c) widget/c)]
  [widget-hidden (->* () (#:attributes attributes/c) widget/c)]
  [widget-text (->* () (#:attributes attributes/c) widget/c)]
  [widget-password (->* () (#:attributes attributes/c) widget/c)]))

(define attributes/c
  (listof (list/c symbol? string?)))

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

(define ((widget-namespace namespace widget-renderer) name widget)
  (widget-renderer (string-append namespace "." name) widget))

(define ((widget-errors #:class [class "errors"]) name value errors)
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

(define (xexpr/optional attribute value)
  (cond
    [value (list (list attribute value))]
    [else null]))

(define ((widget-input #:type [type "text"]
                       #:omit-value? [omit-value? #f]
                       #:attributes [attributes null]) name binding errors)

  (define value (and (not omit-value?) binding (bytes->string/utf-8 (binding:form-value binding))))

  `(input ((type ,type)
           (name ,name)
           ,@attributes
           ,@(xexpr/optional 'value value))))

(define ((widget-textarea #:omit-value? [omit-value? #f]
                          #:attributes [attributes null]) name binding errors)

  (define value (and (not omit-value?) binding (bytes->string/utf-8 (binding:form-value binding))))

  `(textarea ((name ,name) ,@attributes) ,@(xexpr/optional 'value value)))

(define (widget-email #:attributes [attributes null])
  (widget-input #:type "email"
                #:attributes attributes))

(define (widget-file #:attributes [attributes null])
  (widget-input #:type "file"
                #:omit-value? #t
                #:attributes attributes))

(define (widget-hidden #:attributes [attributes null])
  (widget-input #:type "hidden"
                #:attributes attributes))

(define (widget-text #:attributes [attributes null])
  (widget-input #:attributes attributes))

(define (widget-password #:attributes [attributes null])
  (widget-input #:type "password"
                #:omit-value? #t
                #:attributes attributes))
