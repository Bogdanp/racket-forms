#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract/base
         racket/string
         web-server/http
         "contracts.rkt"
         "formlets.rkt"
         "unsafe/prim.rkt")

(provide
 form*

 (contract-out
  [struct form ([constructor (->* () (listof any/c) any/c)]
                [children (listof (cons/c symbol? (or formlet/c form?)))])]
  [form-validate (-> form? bindings/c res/c)]
  [form-process (->* (form? bindings/c) (boolean?) validation/c)]
  [form-run (-> form? request? validation/c)]))

(struct form (constructor children)
  #:transparent)

(define-syntax (form* stx)
  (syntax-parse stx
    [(_ ([name:id f:expr] ...+) e:expr ...+)
     #'(form (lambda (name ...) e ...)
             (list (cons 'name f) ...))]))

(define (validate form bindings namespace)
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
       (define res (validate formlet bindings (string-append namespace (symbol->string name) ".")))
       (cond
         [(ok? res)
          (values (cons (cdr res) results) errors)]

         [else
          (values results (cons (cons name (cdr res)) errors))])]

      [else
       (define full-name (string-append namespace (symbol->string name)))
       (define binding (hash-ref bindings full-name #f))
       (define res (formlet binding))
       (cond
         [(ok? res) (values (cons (cdr res) results) errors)]
         [else (values results (cons (cons name (cdr res)) errors))])])))

(define (form-validate form bindings)
  (validate form bindings ""))

(define (form-process form bindings [submitted? #t])
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

(define (form-run form request #:submit-method [submit-method #"POST"])
  (define submitted? (equal? (request-method request) submit-method))
  (define bindings
    (for/fold ([bindings (hash)])
              ([binding (request-bindings/raw request)])
      (hash-set bindings (bytes->string/utf-8 (binding-id binding)) binding)))

  (form-process form bindings submitted?))
