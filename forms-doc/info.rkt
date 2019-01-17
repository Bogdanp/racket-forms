#lang info

(define collection "forms")

(define scribblings
  '(("forms.scrbl" ())))

(define deps '("base"))
(define build-deps '("forms-lib"
                     "scribble-lib"
                     "racket-doc"))
(define update-implies '("forms-lib"))
