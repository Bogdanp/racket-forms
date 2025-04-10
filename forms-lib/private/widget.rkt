#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/contract/base
         racket/match
         racket/string
         web-server/http
         (only-in xml xexpr/c)
         "contract.rkt")

(provide
 (contract-out
  [widget-namespace (-> string? widget-renderer/c widget-renderer/c)]
  [widget-errors (->* [] [#:class string?] widget/c)]
  [widget-input (widget-> [] [#:type string? #:omit-value? boolean?])]
  [widget-checkbox (widget->)]
  [widget-email (widget->)]
  [widget-file (widget->)]
  [widget-hidden (widget->)]
  [widget-list (-> (-> render-element/c xexpr/c) widget/c)]
  [widget-number (widget->)]
  [widget-password (widget->)]
  [widget-radio-group (widget-> [radio-options/c])]
  [widget-select (widget-> [(or/c (hash/c string? radio-options/c) select-options/c)])]
  [widget-text (widget->)]
  [widget-textarea (widget-> [] [#:omit-value? boolean?])]))

(define-syntax (widget-> stx)
  (syntax-parse stx
    [(_) #'(widget-> [] [])]
    [(_ [required-arg-ctc ...]) #'(widget-> [required-arg-ctc ...] [])]
    [(_ [required-arg-ctc ...] [optional-arg-ctc ...])
     #'(->* [required-arg-ctc ...]
            [optional-arg-ctc ... #:attributes attributes/c]
            widget/c)]))

(define render-element/c
  (->* [widget/c]
       [exact-nonnegative-integer?]
       (or/c xexpr/c (listof xexpr/c))))

(define (lookup-errors errors full-name)
  (let loop ([path (map string->symbol (string-split full-name "."))]
             [errors errors])
    (define current (car path))
    (define remaining (cdr path))
    (define maybe-error
      (and (pair? errors)
           (assq current errors)))
    (cond
      [maybe-error
       (if (null? remaining)
           (cdr maybe-error)
           (loop remaining (cdr maybe-error)))]
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

(define ((widget-list proc) name value errors)
  (define sym (string->symbol name))
  (define seq 0)
  (define (next-id)
    (begin0 seq
      (set! seq (add1 seq))))
  (define the-values (and value (list->vector (reverse value))))
  (define the-errors
    (let ([errors (assq sym errors)])
      (and errors (list->vector (cdr errors)))))
  (define (render-element widget [idx (next-id)])
    (define element-errors
      (cond
        [(and the-errors (idx . < . (vector-length the-errors)))
         (define the-error (vector-ref the-errors idx))
         (if (equal? the-error "") null `((,sym . ,the-error)))]
        [else null]))
    (widget
     #;name name
     #;value (and the-values (vector-ref the-values idx))
     #;errors element-errors))
  (proc render-element))
