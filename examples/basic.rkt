#lang racket

(require forms/base
         racket/match
         web-server/dispatch
         web-server/http)

(struct signup-data (username password))

(define signup-form
  (form* ([username (ensure binding/text (required) (matches #rx".+@.+"))]
          [password (ensure binding/text (required) (longer-than 8))])
   (signup-data username password)))

(define (render-signup-form render-widget)
  `(form ((action "/")
          (method "POST"))
         (label "Username" ,(render-widget "username" (widget-text)))
         ,@(render-widget "username" (widget-errors))
         (label "Password" ,(render-widget "password" (widget-password)))
         ,@(render-widget "password" (widget-errors))
         (button ((type "submit")) "Sign up!")))

(define (signup req)
  (match (form-run signup-form req)
    [`(passed ,_data ,_)
     (response/xexpr '(h1 "Signed up!"))]

    [`(,_ ,_ ,render-widget)
     (response/xexpr (render-signup-form render-widget))]))

(define-values (start _reverse-uri)
  (dispatch-rules
   [("") #:method (or "get" "post") signup]))

(module+ main
  (serve/dispatch start))
