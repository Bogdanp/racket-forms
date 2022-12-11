#lang info

(define license 'BSD-3-Clause)
(define collection "forms")

(define scribblings
  '(("forms.scrbl" ())))

(define deps '("base"))
(define build-deps '("forms-lib"
                     "sandbox-lib"
                     "scribble-lib"
                     "racket-doc"
                     "web-server-doc"
                     "web-server-lib"))
(define update-implies '("forms-lib"))
