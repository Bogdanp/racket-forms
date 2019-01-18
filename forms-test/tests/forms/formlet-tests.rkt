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
    "ensure"

    (test-case "can compose formlets"
      (check-equal? ((ensure text (required)) #f) (err "This field is required."))
      (check-equal? ((ensure text (required)) "a") (ok "a"))
      (check-equal? ((ensure text (required) (longer-than 3)) #f) (err "This field is required."))
      (check-equal? ((ensure text (required) (longer-than 3)) "a") (err "This field must contain 4 or more characters."))
      (check-equal? ((ensure text (required) (longer-than 3)) "abcd") (ok "abcd"))
      (check-equal? ((ensure text (to-number)) #f) (ok #f))
      (check-equal? ((ensure text (to-number)) "a") (err "This field must contain a number."))
      (check-equal? ((ensure text (to-number)) "10.5") (ok 10.5))
      (check-equal? ((ensure text to-boolean) #f) (ok #f))
      (check-equal? ((ensure text to-boolean) "whatever") (ok #t))
      (check-equal? ((ensure text to-symbol) #f) (ok #f))
      (check-equal? ((ensure text to-symbol) "a-b-c") (ok 'a-b-c))))

   (test-suite
    "default"

    (test-case "can assign default values"
      (check-equal? ((ensure text (default "a")) #f) (ok "a"))
      (check-equal? ((ensure text (default "a")) "b") (ok "b"))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests formlet-tests))
