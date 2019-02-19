#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract/base
         racket/hash
         racket/string
         web-server/http
         "contracts.rkt"
         "formlets.rkt"
         "unsafe/prim.rkt")

(provide
 form*

 (contract-out
  [struct form ([constructor any/c]
                [children (listof (cons/c symbol? (or formlet/c form?)))])]
  [form-validate (-> form? bindings/c res/c)]
  [form-process (->* (form? bindings/c) (#:defaults bindings/c #:submitted? boolean?) validation/c)]
  [form-run (->* (form? request?) (#:defaults bindings/c #:submit-methods (listof bytes?)) validation/c)]))

(struct form (constructor children)
  #:transparent)

(define-syntax (form* stx)
  (syntax-parse stx
    [(_ ([name:id f:expr] ...+) e:expr ...+)
     #'(form (lambda (name ...) e ...)
             (list (cons 'name f) ...))]))

(define (validate form bindings namespace)
  (define-values (results errors)
    (for/fold ([results null]
               [errors null])
              ([child (in-list (form-children form))])

      (define name (car child))
      (define formlet (cdr child))
      (define res
        (cond
          [(form? formlet)
           (define form-namespace (string-append namespace (symbol->string name) "."))
           (validate formlet bindings form-namespace)]

          [else
           (define full-name (string-append namespace (symbol->string name)))
           (define binding (hash-ref bindings full-name #f))
           (formlet binding)]))

      (cond
        [(ok? res) (values (cons (cdr res) results) errors)]
        [else (values results (cons (cons name (cdr res)) errors))])))

  (cond
    [(null? errors)
     (define res (apply (form-constructor form) (reverse results)))
     (cond
       [(ok?  res) res]
       [(err? res) res]
       [else   (ok res)])]

    [else (err (reverse errors))]))

(define (form-lookup form full-name)
  (for/fold ([formlet #f]
             [form form]
             #:result formlet)
            ([name (map string->symbol (string-split full-name "."))]
             #:when form)

    (define formlet-pair (assq name (form-children form)))
    (define formlet (and formlet-pair (cdr formlet-pair)))

    (cond
      [(not formlet) (values #f #f)]
      [(form? formlet) (values #f formlet)]
      [else (values formlet #f)])))

(define (form-validate form bindings)
  (validate form bindings ""))

(define (combine/key/keep-newer k _ v) v)

(define (form-process form bindings
                      #:defaults [defaults (hash)]
                      #:submitted? [submitted? #t])
  (define normalized-defaults
    (for/fold ([defaults (hash)])
              ([(name value) defaults])
      (hash-set
       defaults name
       (cond
         [(string? value) (binding:form (string->bytes/utf-8 name) (string->bytes/utf-8 value))]
         [(bytes? value)  (binding:form (string->bytes/utf-8 name) value)]
         [else            value]))))

  (define all-bindings
    (hash-union normalized-defaults bindings #:combine/key combine/key/keep-newer))

  (define ((make-widget-renderer errors) name widget)
    (when (not (form-lookup form name))
      (raise-user-error 'render-widget "Invalid formlet name ~v." name))

    (define binding (hash-ref all-bindings name #f))
    (widget name binding errors))

  (cond
    [submitted?
     (define res (form-validate form bindings))
     (if (ok? res)
         (list 'passed (cdr res) (make-widget-renderer null))
         (list 'failed (cdr res) (make-widget-renderer (cdr res))))]

    [else
     (list 'pending #f (make-widget-renderer null))]))

(define (form-run form request
                  #:defaults [defaults (hash)]
                  #:submit-methods [submit-methods '(#"DELETE" #"PATCH" #"POST" #"PUT")])
  (define submitted? (member (request-method request) submit-methods))
  (define bindings
    (for/fold ([bindings (hash)])
              ([binding (request-bindings/raw request)])
      (hash-set bindings (bytes->string/utf-8 (binding-id binding)) binding)))

  (form-process form bindings #:defaults defaults #:submitted? submitted?))
