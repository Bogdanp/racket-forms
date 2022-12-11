#lang info

(define license 'BSD-3-Clause)
(define collection 'multi)

(define deps '())
(define build-deps '("base"
                     "forms-lib"
                     "rackunit-lib"
                     "srfi-lite-lib"
                     "web-server-lib"))

(define update-implies '("forms-lib"))
