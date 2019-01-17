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
(define validation/c (or/c (list/c 'passed any/c (-> string? widget/c (or/c xexpr/c (listof xexpr/c))))
                           (list/c 'failed any/c (-> string? widget/c (or/c xexpr/c (listof xexpr/c))))
                           (list/c 'pending false/c (-> string? widget/c (or/c xexpr/c (listof xexpr/c))))))


;; Primitives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [ok (-> any/c res/c)]
  [ok? (-> any/c boolean?)]
  [err (-> any/c res/c)]
  [err? (-> any/c boolean?)]
  [check (->* () #:rest (listof (-> any/c res/c)) formlet/c)]
  [~> (->* () #:rest (listof (-> any/c res/c)) formlet/c)]))

(define (ok v)
  (cons 'ok v))

(define (ok? res)
  (and (pair? res) (eq? 'ok (car res))))

(define (err m)
  (cons 'err m))

(define (err? res)
  (and (pair? res) (eq? 'err (car res))))

(define ((and-then fa fb) v)
  (define res (fa v))
  (cond
    [(ok? res) (fb (cdr res))]
    [else res]))

(define (check . fs)
  (for/fold ([g ok])
            ([f fs])
    (and-then g f)))

(define ~> check)


;; Formlets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [text-binding (->* () (#:message string? #:decoder (-> bytes? string?)) formlet/c)]
  [text-field (->* () (#:message string?) formlet/c)]
  [required (->* () (#:message string?) formlet/c)]
  [matches (->* (regexp?) (#:message string?) formlet/c)]
  [shorter-than (->* (exact-positive-integer?) (#:message string?) formlet/c)]
  [longer-than (->* (exact-positive-integer?) (#:message string?) formlet/c)]
  [to-boolean formlet/c]
  [to-number (->* () (#:message string?) formlet/c)]
  [to-symbol formlet/c]

  [boolean formlet/c]
  [text formlet/c]))

(define ((lift f) v)
  (if v
      (f v)
      (ok v)))

(define (text-binding #:message [message "This field must contain some text."]
                      #:decoder [decode bytes->string/utf-8])
  (lift (lambda (v)
          (if (binding:form? v)
              (ok (decode (binding:form-value v)))
              (err message)))))

(define (text-field #:message [message "This field must contain some text."])
  (lift (lambda (v)
          (if (string? v)
              (ok v)
              (err message)))))

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

(define boolean (~> (text-binding) to-boolean))
(define text (text-binding))

(module+ test
  (require rackunit)

  (test-case "primitives"
    (check-equal? ((text-field) #f) (ok #f))
    (check-equal? ((text-field) "a") (ok "a")))

  (test-case "formlets compose with one another"
    (check-equal? (((text-field) . and-then . (required)) #f) (err "This field is required."))
    (check-equal? ((~> (text-field) (required)) "a") (ok "a"))
    (check-equal? ((~> (text-field) (required) (longer-than 3)) #f) (err "This field is required."))
    (check-equal? ((~> (text-field) (required) (longer-than 3)) "a") (err "This field must contain 4 or more characters."))
    (check-equal? ((~> (text-field) (required) (longer-than 3)) "abcd") (ok "abcd"))
    (check-equal? ((~> (text-field) (to-number)) #f) (ok #f))
    (check-equal? ((~> (text-field) (to-number)) "a") (err "This field must contain a number."))
    (check-equal? ((~> (text-field) (to-number)) "10.5") (ok 10.5))
    (check-equal? ((~> (text-field) to-boolean) #f) (ok #f))
    (check-equal? ((~> (text-field) to-boolean) "whatever") (ok #t))
    (check-equal? ((~> (text-field) to-symbol) #f) (ok #f))
    (check-equal? ((~> (text-field) to-symbol) "a-b-c") (ok 'a-b-c))))


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

(module+ test
  (require rackunit)

  (define (make-binding s)
    (binding:form #"" (string->bytes/utf-8 s)))

  (struct login-data (username password)
    #:transparent)

  (define login-form
    (form*
     ([username (~> (text-binding) (required) (matches #rx".+@.+"))]
      [password (~> (text-binding) (required) (longer-than 8))])
     (login-data username password)))

  (define valid-data
    (hash "username" (make-binding "bogdan@example")
          "password" (make-binding "hunter1234")))

  (test-case "simple forms validate their inputs"
    (check-equal?
     (form-validate login-form (hash))
     (err '((username . "This field is required.")
            (password . "This field is required."))))

    (check-equal?
     (form-validate login-form (hash "username" (make-binding "bogdan")
                                     "password" (make-binding "hunter2")))
     (err '((username . "This field must match the regular expression #rx\".+@.+\".")
            (password . "This field must contain 9 or more characters."))))

    (check-equal?
     (form-validate login-form valid-data)
     (ok (login-data "bogdan@example" "hunter1234"))))

  (test-case "simple forms can process their inputs"
    (check-match
     (form-process login-form (hash) #f)
     (list 'pending _ procedure?))

    (check-match
     (form-process login-form valid-data)
     (list 'passed (login-data "bogdan@example" "hunter1234") procedure?))))


;; Widgets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide widget-errors
         widget-input
         widget-text
         widget-password)

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
                                #:required? [required? #f]
                                #:disabled? [disabled? #f]
                                #:min-length [min-length #f]
                                #:max-length [max-length #f]) name binding _)
  (->* () (#:type string?
           #:class (or/c false/c string?)
           #:required? boolean?
           #:disabled? boolean?
           #:min-length (or/c false/c exact-positive-integer?)
           #:max-length (or/c false/c exact-positive-integer?)) widget/c)

  (define (optionally attribute value)
    (cond
      [value (cons attribute value)]
      [else null]))

  `(input ((type ,type)
           (name ,name)
           ,@(optionally "class" class)
           ,@(optionally "required" (and required? "required"))
           ,@(optionally "disabled" (and disabled? "disabled"))
           ,@(optionally "minlength" min-length)
           ,@(optionally "maxlength" max-length)
           (value ,(or (and binding (bytes->string/utf-8 (binding:form-value binding))) "")))))

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
                #:required? required?
                #:disabled? disabled?
                #:min-length min-length
                #:max-length max-length))

(module+ test
  (require racket/match
           rackunit)

  (define (render-login-form render-widget)
    `(form ((action "")
            (method "POST"))
           (label "Username" ,(render-widget "username" (widget-text)))
           ,@(render-widget "username" (widget-errors))
           (label "Password" ,(render-widget "password" (widget-password)))
           ,@(render-widget "password" (widget-errors))))

  (test-case "pending forms can be rendered"
    (match (form-process login-form (hash) #f)
      [(list 'pending _ render-widget)
       (check-equal?
        (render-login-form render-widget)
        '(form ((action "")
                (method "POST"))
               (label "Username" (input ((type "text") (name "username") (value ""))))
               (label "Password" (input ((type "password") (name "password") (value ""))))))]))

  (test-case "failed forms can be rendered"
    (match (form-process login-form (hash "password" (make-binding "hunter1234")))
      [(list 'failed _ render-widget)
       (check-equal?
        (render-login-form render-widget)
        '(form ((action "")
                (method "POST"))
               (label "Username" (input ((type "text") (name "username") (value ""))))
               (ul ((class "errors")) (li "This field is required."))
               (label "Password" (input ((type "password") (name "password") (value "hunter1234"))))))]))

  (test-case "passed forms can be rendered"
    (match (form-process login-form (hash "username" (make-binding "bogdan@example")
                                          "password" (make-binding "hunter1234")))
      [(list 'passed data render-widget)
       (check-equal? data (login-data "bogdan@example" "hunter1234"))
       (check-equal?
        (render-login-form render-widget)
        '(form ((action "")
                (method "POST"))
               (label "Username" (input ((type "text") (name "username") (value "bogdan@example"))))
               (label "Password" (input ((type "password") (name "password") (value "hunter1234"))))))])))
