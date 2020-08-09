#lang racket/base

(require racket/contract/base
         racket/match
         racket/string
         web-server/http
         "contract.rkt")

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
  [widget-radio-group (->* (radio-options/c) (#:attributes attributes/c) widget/c)]
  [widget-select (->* ((or/c (hash/c string? radio-options/c) select-options/c)) (#:attributes attributes/c) widget/c)]
  [widget-text (->* () (#:attributes attributes/c) widget/c)]
  [widget-textarea (->* () (#:omit-value? boolean?
                            #:attributes attributes/c) widget/c)]))

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

(define ((widget-errors #:class [class "errors"]) name _value errors)
  (define (render . error-messages)
    `((ul ([class ,class])
          ,@(for/list ([message (in-list error-messages)])
              `(li ,message)))))

  (match (lookup-errors errors name)
    [(? string? error-message)
     (render error-message)]

    [(? list? error-pairs)
     (apply render (for/list ([pair (in-list error-pairs)])
                     (string-append (car pair) ": " (cdr pair))))]

    [_ null]))

(define (xexpr/optional attribute value)
  (cond
    [value (list (list attribute value))]
    [else null]))

(define ((widget-input #:type [type "text"]
                       #:omit-value? [omit-value? #f]
                       #:attributes [attributes null]) name binding _errors)

  (define value (and (not omit-value?) binding (bytes->string/utf-8 (binding:form-value binding))))

  `(input
    ([type ,type]
     [name ,name]
     ,@attributes
     ,@(xexpr/optional 'value value))))

(define ((widget-checkbox #:attributes [attributes null]) name binding _errors)
  (define value (and binding (bytes->string/utf-8 (binding:form-value binding))))
  (define checked (and value "checked"))

  `(input
    ([type "checkbox"]
     [name ,name]
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

(define ((widget-radio-group options #:attributes [attributes null]) name binding _errors)
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

(define ((widget-select options #:attributes [attributes null]) name binding _errors)
  (define value (and binding (bytes->string/utf-8 (binding:form-value binding))))

  (define (make-option opt-value opt-label)
    (define selected?
      (and value (string=? value opt-value) "selected"))

    `(option
      ([value ,opt-value]
       ,@(xexpr/optional 'selected selected?))
      ,opt-label))

  (define (make-option-group group-label group-opts)
    `(optgroup
      ([label ,group-label])
      ,@(for*/list ([group-option (in-list group-opts)]
                    [opt-value (in-value (car group-option))]
                    [opt-label (in-value (cdr group-option))])
          (make-option opt-value opt-label))))

  (define options-elements
    (cond
      ;; Supported for backwards-compatibility, but discouraged.
      [(hash? options)
       (for/list ([(group-label group-opts) options])
         (make-option-group group-label group-opts))]

      [else
       (for/list ([opt (in-list options)])
         (match opt
           [(cons opt-value (? string? opt-label))
            (make-option opt-value opt-label)]

           [(list (? string? group-label) group-opts)
            (make-option-group group-label group-opts)]))]))

  `(select
    ([name ,name]
     ,@attributes)
    ,@options-elements))

(define (widget-text #:attributes [attributes null])
  (widget-input #:attributes attributes))

(define ((widget-textarea #:omit-value? [omit-value? #f]
                          #:attributes [attributes null]) name binding _errors)
  `(textarea
    ([name ,name]
     ,@attributes)
    ,@(cond
        [omit-value? null]
        [(not binding) null]
        [else (list (bytes->string/utf-8 (binding:form-value binding)))])))
