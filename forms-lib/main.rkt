#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         racket/string
         web-server/http
         (only-in xml
                  xexpr/c))

;; Contracts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define bindings/c (hash/c string? any/c))
(define errors/c (listof (or/c string? (cons/c symbol? (or/c string? (recursive-contract errors/c))))))
(define res/c (cons/c (or/c 'ok 'err) any/c))
(define formlet/c (-> any/c res/c))
(define widget/c (-> string? (or/c false/c binding?) errors/c (or/c xexpr/c (listof xexpr/c))))
(define widget-renderer/c (-> string? widget/c (or/c xexpr/c (listof xexpr/c))))
(define validation/c (or/c (list/c 'passed any/c widget-renderer/c)
                           (list/c 'failed any/c widget-renderer/c)
                           (list/c 'pending false/c widget-renderer/c)))


;; Primitives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [ensure (->* () #:rest (listof formlet/c) formlet/c)]))

(define (ok v)
  (cons 'ok v))

(define (ok? res)
  (and (pair? res) (eq? 'ok (car res))))

(define (err m)
  (cons 'err m))

(define (err? res)
  (and (pair? res) (eq? 'err (car res))))

(define ((fcompose fa fb) v)
  (define res (fa v))
  (cond
    [(ok? res) (fb (cdr res))]
    [else res]))

(define ((alternative fa fb) v)
  (define res (fa v))
  (cond
    [(ok? res) res]
    [else (fb v)]))

(define (ensure . gs)
  (for/fold ([f ok])
            ([g gs])
    (fcompose f g)))


;; Formlets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
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
  (alternative binding/text field/text))

(define email
  (ensure text (matches #rx".+@.+")))


;; Forms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [struct form ([constructor (->* () (listof any/c) any/c)]
                [children (listof (cons/c symbol? (or formlet/c form?)))])])

 form*
 form-validate
 form-process
 form-run)

(struct form (constructor children)
  #:transparent)

(define-syntax (form* stx)
  (syntax-parse stx
    [(_ ([name:id f:expr] ...+) e:expr ...+)
     #'(form (lambda (name ...) e ...)
             (list (cons 'name f) ...))]))

(define/contract (form-validate form bindings [prefix null])
  (->* (form? bindings/c) ((listof string?)) res/c)

  (for/fold ([results null]
             [errors null]
             #:result (if (null? errors)
                          (ok (apply (form-constructor form) (reverse results)))
                          (err (reverse errors))))
            ([child (form-children form)])

    (define name (car child))
    (define formlet (cdr child))

    (cond
      [(form? formlet)
       (define res (form-validate formlet bindings (cons name prefix)))
       (cond
         [(ok? res)
          (values (cons (cdr res) results) errors)]

         [else
          (values results (cons (cons name (cdr res)) errors))])]

      [else
       (define full-name (string-join (map symbol->string (reverse (cons name prefix))) "."))
       (define binding (hash-ref bindings full-name #f))
       (define res (formlet binding))
       (cond
         [(ok? res) (values (cons (cdr res) results) errors)]
         [else (values results (cons (cons name (cdr res)) errors))])])))

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

(define/contract (form-process form bindings [submitted? #t])
  (->* (form? bindings/c) (boolean?) validation/c)

  (define ((make-widget-renderer errors) name widget)
    (widget name (hash-ref bindings name #f) errors))

  (cond
    [submitted?
     (define res (form-validate form bindings))
     (if (ok? res)
         (list 'passed (cdr res) (make-widget-renderer null))
         (list 'failed (cdr res) (make-widget-renderer (cdr res))))]

    [else
     (list 'pending #f (make-widget-renderer null))]))

(define/contract (form-run form request #:submit-method [submit-method #"POST"])
  (-> form? request? validation/c)

  (define submitted? (equal? (request-method request) submit-method))
  (define bindings
    (for/fold ([bindings (hash)])
              ([binding (request-bindings/raw request)])
      (hash-set bindings (bytes->string/utf-8 (binding-id binding)) binding)))

  (form-process form bindings submitted?))


;; Widgets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide widget-namespace
         widget-errors
         widget-input
         widget-email
         widget-text
         widget-password)

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

  (define (optionally attribute value)
    (cond
      [value (list (list attribute value))]
      [else null]))

  (define value
    (and (not omit-value?) binding (bytes->string/utf-8 (binding:form-value binding))))

  `(input ((type ,type)
           (name ,name)
           ,@(optionally 'class class)
           ,@(optionally 'required (and required? "required"))
           ,@(optionally 'disabled (and disabled? "disabled"))
           ,@(optionally 'minlength min-length)
           ,@(optionally 'maxlength max-length)
           ,@(optionally 'value value))))

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
