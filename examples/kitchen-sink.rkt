#lang racket

(require forms/base
         racket/match
         web-server/http
         web-server/dispatch)

(define a-form
  (form* ([an-email (ensure email (required))]
          [a-password (ensure text (required) (longer-than 8))]
          [a-file (ensure binding/file (required))]
          [a-textarea text]
          [a-hidden (ensure text (default "hidden"))])
   (list an-email a-password a-file a-textarea a-hidden)))

(define (render-form render-widget)
  `(form ((action "/")
          (method "POST")
          (enctype "multipart/form-data"))
         (label "Email" ,(render-widget "an-email" (widget-email)))
         ,@(render-widget "an-email" (widget-errors))
         (br)
         (label "Password" ,(render-widget "a-password" (widget-password)))
         ,@(render-widget "a-password" (widget-errors))
         (br)
         (label "File" ,(render-widget "an-file" (widget-file)))
         ,@(render-widget "a-file" (widget-errors))
         (br)
         (label "Textarea" ,(render-widget "a-textarea" (widget-textarea)))
         ,@(render-widget "a-textarea" (widget-errors))
         (br)
         ,(render-widget "a-hidden" (widget-hidden))
         ,@(render-widget "a-hidden" (widget-errors))
         (br)
         (button ((type "submit")) "Sign up!")))

(define (home req)
  (match (form-run a-form req)
    [(list 'passed data _)
     (response/xexpr '(h1 "Signed up!"))]

    [(list _ _ render-widget)
     (response/xexpr (render-form render-widget))]))

(define-values (start reverse-uri)
  (dispatch-rules
   [("") #:method (or "get" "post") home]))

(module+ main
  (serve/dispatch start))
