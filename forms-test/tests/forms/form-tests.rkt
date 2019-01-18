#lang racket/base

(require forms
         racket/match
         racket/string
         rackunit
         web-server/http)

(provide form-tests
         widget-tests)

(define (make-binding s)
  (binding:form #"" (string->bytes/utf-8 s)))

(struct login-data (username password)
  #:transparent)

(define login-form
  (form* ([username (ensure text (required) (matches #rx".+@.+"))]
          [password (ensure text (required) (longer-than 8))])
    (login-data username password)))

(define (render-login-form render-widget)
  `(form ((action "")
          (method "POST"))
         (label "Username" ,(render-widget "username" (widget-text)))
         ,@(render-widget "username" (widget-errors))
         (label "Password" ,(render-widget "password" (widget-password)))
         ,@(render-widget "password" (widget-errors))))

(define valid-login-data
  (hash "username" "bogdan@example"
        "password" "hunter1234"))

(struct author (name email) #:transparent)
(struct package (name version) #:transparent)
(struct release (author package) #:transparent)

(define author-form
  (form* ([name (ensure text (required))]
          [email (ensure email (required))])
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
  (form* ([name (ensure text (required))]
          [version (ensure text (required) parse-version)])
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

(define form-tests
  (test-suite
   "form"

   (test-suite
    "simple"

    (test-case "can validate missing inputs"
      (check-equal?
       (form-validate login-form (hash))
       (err '((username . "This field is required.")
              (password . "This field is required.")))))

    (test-case "can validate bad inputs"
      (check-equal?
       (form-validate login-form (hash "username" "bogdan"
                                       "password" "hunter2"))
       (err '((username . "This field must match the regular expression #rx\".+@.+\".")
              (password . "This field must contain 9 or more characters.")))))

    (test-case "can validate good inputs"
      (check-equal?
       (form-validate login-form valid-login-data)
       (ok (login-data "bogdan@example" "hunter1234"))))

    (test-case "can process pending inputs"
      (check-match
       (form-process login-form (hash) #:submitted? #f)
       (list 'pending _ procedure?)))

    (test-case "can process good inputs"
      (check-match
       (form-process login-form valid-login-data)
       (list 'passed (login-data "bogdan@example" "hunter1234") procedure?))))

   (test-suite
    "composite"

    (test-case "can validate missing inputs"
      (check-equal?
       (form-validate release-form (hash))
       (err '((author . ((name . "This field is required.")
                         (email . "This field is required.")))
              (package . ((name . "This field is required.")
                          (version . "This field is required.")))))))

    (test-case "can validate bad inputs"
      (check-equal?
       (form-validate release-form (hash "author.name" "Bogdan Popa"
                                         "author.email" "bogdan@defn.io"
                                         "package.name" "forms"
                                         "package.version" "a"))
       (err '((package . ((version . "Invalid version.")))))))

    (test-case "can validate good inputs"
      (check-equal?
       (form-validate release-form (hash "author.name" "Bogdan Popa"
                                         "author.email" "bogdan@defn.io"
                                         "package.name" "forms"
                                         "package.version" "1.5.3"))
       (ok (release (author "Bogdan Popa" "bogdan@defn.io")
                    (package "forms" '(1 5 3)))))))))


;; Widgets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define widget-tests
  (test-suite
   "widget"

   (test-suite
    "simple"

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
      (match (form-process login-form (hash "username" (make-binding "bogdan@example")
                                            "password" (make-binding "hunter1234")))
        [(list 'passed data render-widget)
         (check-equal? data (login-data "bogdan@example" "hunter1234"))
         (check-equal?
          (render-login-form render-widget)
          '(form ((action "")
                  (method "POST"))
                 (label "Username" (input ((type "text") (name "username") (value "bogdan@example"))))
                 (label "Password" (input ((type "password") (name "password"))))))])))

   (test-suite
    "complex"

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
                 (button ((type "submit")) "Save")))]))

    (test-case "can render fields' default values"
      (match (form-process (form* [(x text)] x)
                           (hash)
                           #:defaults (hash "x" "default")
                           #:submitted? #f)
        [(list 'pending _ render-widget)
         (check-equal?
          (render-widget "x" (widget-hidden))
          '(input ((type "hidden") (name "x") (value "default"))))

         (check-equal?
          (render-widget "x" (widget-checkbox))
          '(input ((type "checkbox") (name "x") (value "default") (checked "checked"))))]))

    (test-case "raises a user error when attempting to render unknown fields"
      (match (form-process (form* [(x text)] x) (hash) #:submitted? #f)
        [(list 'pending _ render-widget)
         (check-exn exn:fail:user? (lambda ()
                                     (render-widget "y" (widget-text))))])))))

(module+ test
  (require rackunit/text-ui)
  (run-tests (test-suite
              "form"

              form-tests
              widget-tests)))
