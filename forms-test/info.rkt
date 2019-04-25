#lang info

(define collection 'multi)

(define deps '())
(define build-deps '("base"
                     "forms-lib"
                     "rackunit-lib"
                     "srfi-lite-lib"
                     "web-server-lib"))

(define update-implies '("forms-lib"))
