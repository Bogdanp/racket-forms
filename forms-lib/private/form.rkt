#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract/base
         racket/hash
         racket/match
         racket/string
         web-server/http
         "contract.rkt"
         (submod "contract.rkt" internal)
         (submod "prim.rkt" unsafe))

(provide
 form*

 (contract-out
  [struct form ([constructor any/c]
                [children (listof (cons/c symbol? (or/c formlet/c form?)))])]
  [form-validate (-> form? bindings/c res/c)]
  [form-process (->* (form? bindings/c) (#:defaults bindings/c #:submitted? boolean?) validation-result/c)]
  [form-run (->* (form? request?) (#:defaults bindings/c #:submit-methods (listof bytes?)) validation-result/c)]))

(struct form (constructor children)
  #:transparent)

(define-syntax (form* stx)
  (syntax-parse stx
    [(_ ([name:id f:expr] ...) e:expr ...+)
     #'(form (lambda (name ...) e ...)
             (list (cons 'name f) ...))]))

(define (validate f bindings namespace)
  (define-values (results errors)
    (for/fold ([results null]
               [errors null])
              ([child (in-list (form-children f))])

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
     (define res (apply (form-constructor f) (reverse results)))
     (cond
       [(ok?  res) res]
       [(err? res) res]
       [else   (ok res)])]

    [else (err (reverse errors))]))

(define (form-lookup f full-name)
  (define names
    (map string->symbol (string-split full-name ".")))
  (for/fold ([formlet #f]
             [subform f]
             #:result formlet)
            ([name (in-list names)]
             #:when subform)
    (match (assq name (form-children subform))
      [#f (values #f #f)]
      [(cons _ (? form? subform)) (values #f subform)]
      [(cons _ formlet) (values formlet #f)])))

(module+ internal
  (provide form-lookup))

(define (form-validate f bindings)
  (validate f bindings ""))

(define (combine/key/keep-newer _k1 _k2 v) v)

(define (form-process f bindings
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
    (when (not (form-lookup f name))
      (raise-user-error 'render-widget "Invalid formlet name ~v." name))

    (define binding (hash-ref all-bindings name #f))
    (widget name binding errors))

  (cond
    [submitted?
     (define res (form-validate f bindings))
     (if (ok? res)
         (list 'passed (cdr res) (make-widget-renderer null))
         (list 'failed (cdr res) (make-widget-renderer (cdr res))))]

    [else
     (list 'pending #f (make-widget-renderer null))]))

(define (form-run f request
                  #:defaults [defaults (hash)]
                  #:submit-methods [submit-methods '(#"DELETE" #"PATCH" #"POST" #"PUT")])
  (define submitted? (member (request-method request) submit-methods))
  (define bindings
    (for/fold ([bindings (hash)])
              ([binding (request-bindings/raw request)])
      (hash-set bindings (bytes->string/utf-8 (binding-id binding)) binding)))

  (form-process f bindings #:defaults defaults #:submitted? submitted?))
