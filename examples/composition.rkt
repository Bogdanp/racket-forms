#lang racket

(require forms
         racket/match
         web-server/http
         web-server/dispatch)

(struct author (name email) #:transparent)
(struct package (name version) #:transparent)
(struct release (author package) #:transparent)

(define author-form
  (form* ([name (ensure text (required) (longer-than 3))]
          [email email])
    (author name email)))

(define (parse-version v)
  (define version (map string->number (string-split v ".")))
  (if (member #f version)
      (err "Invalid version.")
      (ok version)))

(define package-form
  (form* ([name (ensure text (required) (longer-than 3))]
          [version (ensure text (required) (longer-than 1) parse-version)])
    (package name version)))

(define release-form
  (form* ([author author-form]
          [package package-form])
    (release author package)))

(define (render-author-form render-widget)
  `(div (label "Name" ,(render-widget "name" (widget-text)))
        ,@(render-widget "name" (widget-errors))
        (br)
        (label "Email" ,(render-widget "email" (widget-email)))
        ,@(render-widget "email" (widget-errors))))

(define (render-package-form render-widget)
  `(div (label "Name" ,(render-widget "name" (widget-text)))
        ,@(render-widget "name" (widget-errors))
        (br)
        (label "Version" ,(render-widget "version" (widget-text)))
        ,@(render-widget "version" (widget-errors))))

(define (render-release-form render-widget)
  `(form ((action "")
          (method "POST"))
         (fieldset
          (legend "Author")
          ,(render-author-form (widget-namespace "author" render-widget)))
         (fieldset
          (legend "Package")
          ,(render-package-form (widget-namespace "package" render-widget)))
         (button ((type "submit")) "Save")))

(define (cut-release req)
  (match (form-run release-form req)
    [(list 'passed data _)
     (response/xexpr '(h1 "Release cut!"))]

    [(list _ _ render-widget)
     (response/xexpr (render-release-form render-widget))]))

(define-values (start reverse-uri)
  (dispatch-rules
   [("") #:method (or "get" "post") cut-release]))

(module+ main
  (serve/dispatch start))
