#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
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
  [struct form
    ([constructor any/c]
     [children (listof (cons/c symbol? (or/c formlet/c form?)))])]
  [form-validate
   (-> form? bindings/c res/c)]
  [form-process
   (->*
    [form? bindings/c]
    [#:combine (-> any/c any/c any/c any/c)
     #:defaults bindings/c
     #:submitted? boolean?]
    validation-result/c)]
  [form-run
   (->*
    [form? request?]
    [#:combine (-> any/c any/c any/c any/c)
     #:defaults bindings/c
     #:submit-methods (listof bytes?)]
    validation-result/c)]))

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
      (match-define (cons name formlet) child)
      (define res
        (cond
          [(form? formlet)
           (define form-namespace (format "~a." name))
           (validate formlet bindings form-namespace)]
          [else
           (define full-name (format "~a~a" namespace name))
           (define binding (hash-ref bindings full-name #f))
           (formlet binding)]))
      (cond
        [(ok? res) (values (cons (cdr res) results) errors)]
        [else (values results (cons (cons name (cdr res)) errors))])))
  (cond
    [(null? errors)
     (define res
       (apply
        (form-constructor f)
        (reverse results)))
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

(define (form-process f bindings
                      #:combine [combine-proc (λ (_k _v1 v2) v2)]
                      #:defaults [defaults (hash)]
                      #:submitted? [submitted? #t])
  (define normalized-defaults
    (for/fold ([res (hash)])
              ([(name value) (in-hash defaults)])
      (hash-set
       res name
       (cond
         [(string? value) (binding:form (string->bytes/utf-8 name) (string->bytes/utf-8 value))]
         [(bytes? value)  (binding:form (string->bytes/utf-8 name) value)]
         [else            value]))))

  (define all-bindings
    (hash-union
     #:combine/key combine-proc
     normalized-defaults bindings))

  (define ((make-widget-renderer errors) name widget)
    (when (not (form-lookup f name))
      (raise-user-error 'render-widget "formlet not found: ~v" name))
    (define binding (hash-ref all-bindings name #f))
    (widget name binding errors))

  (cond
    [submitted?
     (define res
       (form-validate f bindings))
     (if (ok? res)
         `(passed ,(cdr res) ,(make-widget-renderer null))
         `(failed ,(cdr res) ,(make-widget-renderer (cdr res))))]
    [else
     `(pending #f ,(make-widget-renderer null))]))

(define (form-run f request
                  #:combine [combine-proc (λ (_k _v1 v2) v2)]
                  #:defaults [defaults (hash)]
                  #:submit-methods [submit-methods '(#"DELETE" #"PATCH" #"POST" #"PUT")])
  (define submitted?
    (member (request-method request) submit-methods))
  (define bindings
    (for/fold ([bindings (hash)])
              ([binding (in-list (request-bindings/raw request))])
      (define name (bytes->string/utf-8 (binding-id binding)))
      (if (hash-has-key? bindings name)
          (hash-set bindings name (combine-proc name (hash-ref bindings name) binding))
          (hash-set bindings name binding))))
  (form-process
   #:combine combine-proc
   #:defaults defaults
   #:submitted? submitted?
   f bindings))
