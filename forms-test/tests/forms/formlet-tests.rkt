#lang racket/base

(require forms
         rackunit
         web-server/http)

(provide formlet-tests)

(define (make-binding s)
  (binding:form #"" (string->bytes/utf-8 s)))


(define formlet-tests
  (test-suite
   "formlet"

   (test-suite
    "text"
    (test-case "can handle #f, strings and bindings"
      (check-equal? (text #f) (ok #f))
      (check-equal? (text "a") (ok "a"))
      (check-equal? (text (make-binding "a")) (ok "a"))))

   (test-suite
    "required"

    (test-case "can error given #f"
      (check-equal? ((ensure text (required)) #f) (err "This field is required.")))

    (test-case "can error given an empty string"
      (check-equal? ((ensure text (required)) "") (err "This field is required."))))

   (test-suite
    "one-of"

    (test-case "can limit the set of inputs"
      (check-equal?
       ((ensure text (one-of '(("a" . a)
                               ("b" . b)))) "c")
       (err "This field must contain one of the following values: a, b")))

    (test-case "can map value tags to concrete values"
      (check-equal?
       ((ensure text (one-of '(("a" . concrete)))) "a") (ok 'concrete))))

   (test-suite
    "ensure"

    (test-case "can compose formlets"
      (check-equal? ((ensure text (required) (longer-than 3)) #f) (err "This field is required."))
      (check-equal? ((ensure text (required) (longer-than 3)) "a") (err "This field must contain 4 or more characters."))
      (check-equal? ((ensure text (required) (longer-than 3)) "abcd") (ok "abcd"))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests formlet-tests))
