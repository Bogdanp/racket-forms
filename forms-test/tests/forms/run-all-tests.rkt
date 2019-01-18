#lang racket/base

(require rackunit
         rackunit/text-ui)

(require "formlet-tests.rkt"
         "form-tests.rkt")

(define all-tests
  (test-suite
   "forms-lib"

   formlet-tests
   form-tests
   widget-tests))

(module+ main
  (run-tests all-tests))
