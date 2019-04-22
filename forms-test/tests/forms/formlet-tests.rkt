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
    "binding/text"

    (test-case "can handle #f and bindings"
      (check-equal? (binding/text #f) (ok #f))
      (check-equal? (binding/text (make-binding "a")) (ok "a")))

    (test-case "handles empty strings as #f"
      (check-equal? (binding/text (make-binding "")) (ok #f))))

   (test-suite
    "binding/boolean"

    (test-case "can handle any value"
      (check-equal? (binding/boolean #f) (ok #f))
      (check-equal? (binding/boolean (make-binding "")) (ok #f))
      (check-equal? (binding/boolean (make-binding "a")) (ok #t))))

   (test-suite
    "binding/email"

    (test-case "can validate email addresses"
      (check-equal? (binding/email #f) (ok #f))
      (check-equal? (binding/email (make-binding "")) (ok #f))
      (check-equal? (binding/email (make-binding "a")) (err "This field must contain an e-mail address."))
      (check-equal? (binding/email (make-binding "bogdan@example.com")) (ok "bogdan@example.com"))))

   (test-suite
    "binding/number"

    (test-case "can validate numbers"
      (check-equal? (binding/number #f) (ok #f))
      (check-equal? (binding/number (make-binding "")) (ok #f))
      (check-equal? (binding/number (make-binding "a")) (err "This field must contain a number."))
      (check-equal? (binding/number (make-binding "42")) (ok 42))
      (check-equal? (binding/number (make-binding "3.14159")) (ok 3.14159))))

   (test-suite
    "binding/symbol"

    (test-case "can turn strings into symbols"
      (check-equal? (binding/symbol #f) (ok #f))
      (check-equal? (binding/symbol (make-binding "")) (ok #f))
      (check-equal? (binding/symbol (make-binding "a")) (ok 'a))
      (check-equal? (binding/symbol (make-binding "foo-bar-baz")) (ok 'foo-bar-baz))
      (check-equal? (binding/symbol (make-binding "a long symbol")) (ok '|a long symbol|))))

   (test-suite
    "required"

    (test-case "can error given #f"
      (check-equal? ((ensure binding/text (required)) #f) (err "This field is required.")))

    (test-case "can error given an empty string"
      (check-equal? ((ensure binding/text (required)) (make-binding "")) (err "This field is required."))))

   (test-suite
    "one-of"

    (test-case "can limit the set of inputs"
      (check-equal?
       ((ensure binding/text (one-of '(("a" . a)
                                       ("b" . b)))) (make-binding "c"))
       (err "This field must contain one of the following values: a, b")))

    (test-case "can map value tags to concrete values"
      (check-equal?
       ((ensure binding/text (one-of '(("a" . concrete)))) (make-binding "a")) (ok 'concrete))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests formlet-tests))
