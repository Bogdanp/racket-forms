#lang racket/base

(require rackunit
         rackunit/text-ui)

(require "forms-tests.rkt")

(module+ main
  (run-tests forms-tests))
