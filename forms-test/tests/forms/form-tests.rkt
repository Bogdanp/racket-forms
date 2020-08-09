#lang racket/base

(require forms
         (submod forms/private/form internal)
         racket/match
         racket/string
         rackunit
         (only-in web-server/http make-binding:form)
         "util.rkt")

(provide form-tests)

(define empty-form
  (form* () #f))

(struct signup-data (username password)
  #:transparent)

(define signup-form
  (form* ([username (ensure binding/email (required))]
          [password (form* ([p1 (ensure binding/text (required) (longer-than 8))]
                            [p2 (ensure binding/text (required) (longer-than 8))])
                      (cond
                        [(string=? p1 p2) (ok p1)]
                        [else (err "The passwords must match.")]))])
    (signup-data username password)))

(struct login-data (username password)
  #:transparent)

(define login-form
  (form* ([username (ensure binding/email (required))]
          [password (ensure binding/text (required) (longer-than 8))])
    (login-data username password)))

(define (render-login-form render-widget)
  `(form ((action "")
          (method "POST"))
         (label "Username" ,(render-widget "username" (widget-text)))
         ,@(render-widget "username" (widget-errors))
         (label "Password" ,(render-widget "password" (widget-password)))
         ,@(render-widget "password" (widget-errors))))

(define valid-login-data
  (hash "username" (make-binding "bogdan@example")
        "password" (make-binding "hunter1234")))

(struct author (name email) #:transparent)
(struct package (name version) #:transparent)
(struct release (author package) #:transparent)

(define author-form
  (form* ([name (ensure binding/text (required))]
          [email (ensure binding/email (required))])
    (author name email)))

(define (render-author-form render-widget)
  `(div (label "Name" ,(render-widget "name" (widget-text)))
        ,@(render-widget "name" (widget-errors))
        (label "Email" ,(render-widget "email" (widget-email)))
        ,@(render-widget "email" (widget-errors))))

(define (parse-version v)
  (define version (map string->number (string-split v ".")))
  (if (member #f version)
      (err "Invalid version.")
      (ok version)))

(define package-form
  (form* ([name (ensure binding/text (required))]
          [version (ensure binding/text (required) parse-version)])
    (package name version)))

(define (render-package-form render-widget)
  `(div (label "Name" ,(render-widget "name" (widget-text)))
        ,@(render-widget "name" (widget-errors))
        (label "Version" ,(render-widget "version" (widget-text)))
        ,@(render-widget "version" (widget-errors))))

(define release-form
  (form* ([author author-form]
          [package package-form])
    (release author package)))

(define (render-release-form render-widget)
  `(form ((action "")
          (method "POST"))
         (fieldset
          (legend "Author")
          ,(render-author-form (widget-namespace "author" render-widget)))
         (fieldset
          (legend "Package")
          ,(render-package-form (widget-namespace "package" render-widget)))
         (button ((type "submit")) "Save")))

(define valid-release-data
  (hash "author.name" (make-binding "Bogdan Popa")
        "author.email" (make-binding "bogdan@defn.io")
        "package.name" (make-binding "forms")
        "package.version" (make-binding "1.5.3")))

(define form-tests
  (test-suite
   "form"

   (test-suite
    "form-lookup"

    (test-case "can lookup formlets at any depth"
      (check-false (form-lookup login-form "blah"))
      (check-not-false (form-lookup login-form "username"))
      (check-not-false (form-lookup signup-form "password.p1"))))

   (test-suite
    "form-validate"

    (test-case "can validate missing inputs"
      (check-equal?
       (form-validate login-form (hash))
       (err '((username . "This field is required.")
              (password . "This field is required."))))

      (check-equal?
       (form-validate release-form (hash))
       (err '((author . ((name . "This field is required.")
                         (email . "This field is required.")))
              (package . ((name . "This field is required.")
                          (version . "This field is required.")))))))

    (test-case "can validate bad inputs"
      (check-equal?
       (form-validate login-form (hash "username" (make-binding "bogdan")
                                       "password" (make-binding "hunter2")))
       (err '((username . "This field must contain an e-mail address.")
              (password . "This field must contain 9 or more characters."))))

      (check-equal?
       (form-validate release-form (hash-set valid-release-data "package.version" (make-binding "a")))
       (err '((package . ((version . "Invalid version.")))))))

    (test-case "can validate good inputs"
      (check-equal?
       (form-validate login-form valid-login-data)
       (ok (login-data "bogdan@example" "hunter1234")))

      (check-equal?
       (form-validate release-form valid-release-data)
       (ok (release (author "Bogdan Popa" "bogdan@defn.io")
                    (package "forms" '(1 5 3))))))

    (test-case "can validate nested inputs"
      (check-equal?
       (form-validate signup-form (hash "username" (make-binding "bogdan@defn.io")))
       (err '((password (p1 . "This field is required.")
                        (p2 . "This field is required.")))))

      (check-equal?
       (form-validate signup-form (hash "username"    (make-binding "bogdan@defn.io")
                                        "password.p1" (make-binding "password-123-a")
                                        "password.p2" (make-binding "password-123-b")))
       (err '((password . "The passwords must match."))))

      (check-equal?
       (form-validate signup-form (hash "username"    (make-binding "bogdan@defn.io")
                                        "password.p1" (make-binding "password-123-a")
                                        "password.p2" (make-binding "password-123-a")))
       (ok (signup-data "bogdan@defn.io" "password-123-a")))))

   (test-suite
    "form-process"

    (test-case "can process pending inputs"
      (check-match
       (form-process login-form (hash) #:submitted? #f)
       (list 'pending _ procedure?)))

    (test-case "can process good inputs"
      (check-match
       (form-process login-form valid-login-data)
       (list 'passed (login-data "bogdan@example" "hunter1234") procedure?)))

    (test-case "can render pending forms"
      (match (form-process login-form (hash) #:submitted? #f)
        [(list 'pending _ render-widget)
         (check-equal?
          (render-login-form render-widget)
          '(form ((action "")
                  (method "POST"))
                 (label "Username" (input ((type "text") (name "username"))))
                 (label "Password" (input ((type "password") (name "password"))))))]))

    (test-case "can render failed forms"
      (match (form-process login-form (hash "password" (make-binding "hunter1234")))
        [(list 'failed _ render-widget)
         (check-equal?
          (render-login-form render-widget)
          '(form ((action "")
                  (method "POST"))
                 (label "Username" (input ((type "text") (name "username"))))
                 (ul ((class "errors")) (li "This field is required."))
                 (label "Password" (input ((type "password") (name "password"))))))]))

    (test-case "can render passed forms"
      (match (form-process login-form valid-login-data)
        [(list 'passed data render-widget)
         (check-equal? data (login-data "bogdan@example" "hunter1234"))
         (check-equal?
          (render-login-form render-widget)
          '(form ((action "")
                  (method "POST"))
                 (label "Username" (input ((type "text") (name "username") (value "bogdan@example"))))
                 (label "Password" (input ((type "password") (name "password"))))))])))

   (test-suite
    "form-process+widget-renderer"

    (test-case "can render pending forms"
      (match (form-process release-form (hash) #:submitted? #f)
        [(list 'pending _ render-widget)
         (check-equal?
          (render-release-form render-widget)
          '(form ((action "")
                  (method "POST"))
                 (fieldset
                  (legend "Author")
                  (div
                   (label "Name" (input ((type "text") (name "author.name"))))
                   (label "Email" (input ((type "email") (name "author.email"))))))
                 (fieldset
                  (legend "Package")
                  (div
                   (label "Name" (input ((type "text") (name "package.name"))))
                   (label "Version" (input ((type "text") (name "package.version"))))))
                 (button ((type "submit")) "Save")))])))

   (test-suite
    "form-run"

    (test-case "treats GET requests as not submitted"
      (match (form-run release-form (make-request))
        [(list res _ _)
         (check-equal? res 'pending)]))

    (test-case "treats POST requests as submitted"
      (match (form-run login-form (make-request #:method #"POST"
                                                #:bindings (list (make-binding:form #"username" #"bogdan@example.com")
                                                                 (make-binding:form #"password" #"hunter1234"))))
        [(list res _ _)
         (check-equal? res 'passed)])))))

(module+ test
  (require rackunit/text-ui)
  (run-tests form-tests))
