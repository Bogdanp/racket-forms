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
  [widget-checkbox (->* () (#:attributes attributes/c) widget/c)]
  [widget-email (->* () (#:attributes attributes/c) widget/c)]
  [widget-file (->* () (#:attributes attributes/c) widget/c)]
  [widget-hidden (->* () (#:attributes attributes/c) widget/c)]
  [widget-number (->* () (#:attributes attributes/c) widget/c)]
  [widget-password (->* () (#:attributes attributes/c) widget/c)]
  [widget-radio-group (->* (options/c) (#:attributes attributes/c) widget/c)]
  [widget-select (->* ((or/c (hash/c string? options/c) options/c)) (#:attributes attributes/c) widget/c)]
  [widget-text (->* () (#:attributes attributes/c) widget/c)]
  [widget-textarea (->* () (#:omit-value? boolean?
                            #:attributes attributes/c) widget/c)]))

(define attributes/c
  (listof (list/c symbol? string?)))

(define options/c
  (listof (cons/c string? string?)))

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

(define ((widget-checkbox #:attributes [attributes null]) name binding errors)
  (define value (and binding (bytes->string/utf-8 (binding:form-value binding))))
  (define checked (and value "checked"))

  `(input ((type "checkbox")
           (name ,name)
           ,@attributes
           ,@(xexpr/optional 'value value)
           ,@(xexpr/optional 'checked checked))))

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

(define (widget-number #:attributes [attributes null])
  (widget-input #:type "number"
                #:attributes attributes))

(define (widget-password #:attributes [attributes null])
  (widget-input #:type "password"
                #:omit-value? #t
                #:attributes attributes))

(define ((widget-radio-group options #:attributes [attributes null]) name binding errors)
  (define value (and binding (bytes->string/utf-8 (binding:form-value binding))))

  (define (make-radio option)
    (define radio-value (car option))
    (define radio-label (cdr option))
    (define checked (and value (string=? value radio-value) "checked"))

    `(label
      (input
       ((type "radio")
        (name ,name)
        ,@attributes
        ,@(xexpr/optional 'value radio-value)
        ,@(xexpr/optional 'checked checked)))
      ,radio-label))

  `(div ,@(map make-radio options)))

(define ((widget-select options #:attributes [attributes null]) name binding errors)
  (define value (and binding (bytes->string/utf-8 (binding:form-value binding))))

  (define (make-option option)
    (define option-value (car option))
    (define option-label (cdr option))
    (define selected? (and value (string=? value option-value) "selected"))

    `(option
      ((value ,option-value)
       ,@(xexpr/optional 'selected selected?)) ,option-label))

  (define options-elements
    (cond
      [(hash? options)
       (for/list ([(group-label options) options])
         `(optgroup ((label ,group-label)) ,@(map make-option options)))]

      [else
       (map make-option options)]))

  `(select ((name ,name) ,@attributes) ,@options-elements))

(define (widget-text #:attributes [attributes null])
  (widget-input #:attributes attributes))

(define ((widget-textarea #:omit-value? [omit-value? #f]
                          #:attributes [attributes null]) name binding errors)

  (define value (and (not omit-value?) binding (bytes->string/utf-8 (binding:form-value binding))))
  (define value/xexpr (or (and value (list value)) null))

  `(textarea ((name ,name) ,@attributes) ,@value/xexpr))
