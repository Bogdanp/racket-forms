#lang racket

(require forms/base
         racket/match
         web-server/http
         web-server/dispatch)

(define file-form
  (form* ([email (ensure email (required))]
          [file (ensure binding/file (required))])
   (list email file)))

(define (render-file-form render-widget)
  `(form ((action "/")
          (method "POST")
          (enctype "multipart/form-data"))
         (label "Email" ,(render-widget "email" (widget-email)))
         ,@(render-widget "email" (widget-errors))
         (label "File" ,(render-widget "file" (widget-file)))
         ,@(render-widget "file" (widget-errors))
         (button ((type "submit")) "Sign up!")))

(define (upload req)
  (match (form-run file-form req)
    [(list 'passed data _)
     (response/xexpr '(h1 "File uploaded!"))]

    [(list _ _ render-widget)
     (response/xexpr (render-file-form render-widget))]))

(define-values (start reverse-uri)
  (dispatch-rules
   [("") #:method (or "get" "post") upload]))

(module+ main
  (serve/dispatch start))
