#lang racket/base

(require forms
         racket/match
         rackunit)

(provide
 widget-tests)

(define simple-form
  (form* [(x binding/text)]
    x))

(define (test-renderer fn
                       #:form [form simple-form]
                       #:bindings [bindings (hash)]
                       #:defaults [defaults (hash)]
                       #:submitted? [submitted? #f])
  (match (form-process form bindings #:defaults defaults #:submitted? submitted?)
    [(list _ _ render-widget)
     (fn render-widget)]))

(define widget-tests
  (test-suite
   "widget"

   (test-suite
    "render-widget"

    (test-case "raises a user error when attempting to render unknown fields"
      (test-renderer
       (lambda (render-widget)
         (check-exn
          exn:fail:user?
          (lambda ()
            (render-widget "idontexist" (widget-text))))))))

   (test-suite
    "widget-input"

    (test-case "can render fields' default values"
      (test-renderer
       #:defaults (hash "x" "default")
       (lambda (render-widget)
         (check-equal?
          (render-widget "x" (widget-hidden))
          '(input ((type "hidden") (name "x") (value "default"))))

         (check-equal?
          (render-widget "x" (widget-checkbox))
          '(input ((type "checkbox") (name "x") (value "default") (checked "checked"))))))))

   (test-suite
    "widget-radio-group"

    (test-case "can render simple option lists"
      (test-renderer
       (lambda (render-widget)
         (check-equal?
          (render-widget "x" (widget-radio-group '(("cat" . "Cat")
                                                   ("dog" . "Dog"))))
          '(div
            (label (input ((type "radio") (name "x") (value "cat"))) "Cat")
            (label (input ((type "radio") (name "x") (value "dog"))) "Dog")))))

      (test-renderer
       #:defaults (hash "x" "cat")
       (lambda (render-widget)
         (check-equal?
          (render-widget "x" (widget-radio-group '(("cat" . "Cat")
                                                   ("dog" . "Dog"))))
          '(div
            (label (input ((type "radio") (name "x") (value "cat") (checked "checked"))) "Cat")
            (label (input ((type "radio") (name "x") (value "dog"))) "Dog")))))))

   (test-suite
    "widget-select"

    (test-case "can render simple option lists"
      (test-renderer
       (lambda (render-widget)
         (check-equal?
          (render-widget "x" (widget-select '(("cat" . "Cat")
                                              ("dog" . "Dog"))))
          '(select
            ((name "x"))
            (option ((value "cat")) "Cat")
            (option ((value "dog")) "Dog")))))

      (test-renderer
       #:defaults (hash "x" "cat")
       (lambda (render-widget)
         (check-equal?
          (render-widget "x" (widget-select '(("cat" . "Cat")
                                              ("dog" . "Dog"))))
          '(select
            ((name "x"))
            (option ((value "cat") (selected "selected")) "Cat")
            (option ((value "dog")) "Dog"))))))

    (test-case "can render option groups"
      (test-renderer
       (lambda (render-widget)
         (check-equal?
          (render-widget "x" (widget-select (hash "Animals" '(("cat" . "Cat")
                                                              ("dog" . "Dog")))))
          '(select
            ((name "x"))
            (optgroup
             ((label "Animals"))
             (option ((value "cat")) "Cat")
             (option ((value "dog")) "Dog")))))))

    (test-case "can render ordered option groups"
      (test-renderer
       (lambda (render-widget)
         (check-equal?
          (render-widget "x" (widget-select (list (cons "first" "First")
                                                  (list "Animals"
                                                        '(("cat" . "Cat")
                                                          ("dog" . "Dog")))
                                                  (list "Places"
                                                        '(("asia" . "Asia")
                                                          ("europe" . "Europe"))))))
          '(select
            ((name "x"))
            (option ([value "first"]) "First")
            (optgroup
             ([label "Animals"])
             (option ([value "cat"]) "Cat")
             (option ([value "dog"]) "Dog"))
            (optgroup
             ([label "Places"])
             (option ([value "asia"]) "Asia")
             (option ([value "europe"]) "Europe"))))))))

   (test-suite
    "widget-textarea"

    (test-case "can render a textarea"
      (test-renderer
       (lambda (render-widget)
         (check-equal?
          (render-widget "x" (widget-textarea))
          '(textarea ((name "x")))))))

    (test-case "can renderer textarea values"
      (test-renderer
       #:defaults (hash "x" "hello!")
       (lambda (render-widget)
         (check-equal?
          (render-widget "x" (widget-textarea))
          '(textarea ((name "x")) "hello!"))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests widget-tests))
