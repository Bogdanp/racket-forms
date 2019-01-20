#lang racket

(require forms/base
         net/base64
         racket/function
         racket/random
         web-server/dispatch
         web-server/http
         web-server/http/redirect)

(define (slugify title)
  (regexp-replace* #px"[^a-zA-Z0-9\\-_]" (string-downcase title) "-"))

(struct author (name email))

(define author-form
  (form* ([name (ensure binding/text (required) (longer-than 3) (shorter-than 50))]
          [email (ensure binding/email (required))])
    (author name email)))

(struct article (author title slug content))

(define article-form
  (form* ([author author-form]
          [title (ensure binding/text (required) (longer-than 1) (shorter-than 150))]
          [content (ensure binding/text (required) (longer-than 1))])
    (article author title (slugify title) content)))

(define articles
  (box (list (article (author "Bogdan Popa" "bogdan@defn.io") "Part Two" "part-two" "Hi, it's me again!")
             (article (author "Bogdan Popa" "bogdan@defn.io") "Hello World!" "hello-world-" "Hi there, world!"))))

(define (article-lookup slug)
  (for/first ([article (unbox articles)]
              #:when (string=? (article-slug article) slug))
    article))

(define (article-add! article)
  (box-swap! articles (lambda (articles)
                        (cons article articles))))

(define (article-replace! slug new-article)
  (box-swap! articles (lambda (articles)
                        (for/list ([article articles])
                          (if (string=? (article-slug article) slug)
                              new-article
                              article)))))

;; This is not that important for this example so you can ignore it.
;; It atomically swaps the contents of a box.
(define (box-swap! box f . args)
  (let loop ()
    (let* ([v0 (unbox box)]
           [v1 (apply f v0 args)])
      (if (box-cas! box v0 v1) v1 (loop)))))

;; Ditto for all the CSRF token stuff.  Since we're not using
;; continuations to process the forms, we need CSRF tokens to validate
;; that the user actually intended to make a request.
(define csrf-token (make-parameter #f))

(define (render-csrf-token)
  `(input ((type "hidden") (name "csrf_token") (value ,(csrf-token)))))

(define ((make-labeled-field label name widget) render-widget)
  `(div
    ((class "form__group"))
    (label ,label ,(render-widget name widget))
    ,@(render-widget name (widget-errors #:class "form__errors"))))

(define (render-author-form render-widget)
  (define render-name-field (make-labeled-field "Name" "name" (widget-text)))
  (define render-email-field (make-labeled-field "Email" "email" (widget-email)))

  `(fieldset
    ((class "form__fieldset"))
    (legend "Author")
    ,(render-name-field render-widget)
    ,(render-email-field render-widget)))

(define (render-article-form render-widget [action-label "Save"])
  (define author-form (render-author-form (widget-namespace "author" render-widget)))
  (define render-title-field (make-labeled-field "Title" "title" (widget-text)))
  (define render-content-field (make-labeled-field "Content" "content" (widget-textarea #:attributes '((columns "140")
                                                                                                       (rows "5")))))
  (define content-form `(fieldset
                         ((class "form__fieldset"))
                         (legend "Article")
                         ,(render-title-field render-widget)
                         ,(render-content-field render-widget)))

  `(form
    ((action "")
     (method "POST")
     (class "form"))
    ,author-form
    ,content-form
    ,(render-csrf-token)
    (button ((type "submit")) ,action-label)))

(define (render-article article)
  `(div
    ((class "article"))
    (h2
     ((class "article__title"))
     (a
      ((href ,(reverse-uri view-article (article-slug article))))
      ,(article-title article)))
    (h5
     ((class "article__subtitle"))
     "Posted by " ,(author-name (article-author article)) " "
     (a ((href ,(reverse-uri edit-article (article-slug article)))) "Edit"))
    (div
     ((class "article__content"))
     ,(article-content article))))

(define (render-template . content)
  (define (render-nav-item label uri)
    `(li
      ((class "nav__items"))
      (a ((href ,uri)) ,label)))

  (response/xexpr
   #:preamble #"<!DOCTYPE html>"
   `(html
     (head
      (title "Blag"))
     (body
      (div
       ((class "nav"))
       (ul
        ((class "nav__items"))
        ,(render-nav-item "Home" (reverse-uri home))
        ,(render-nav-item "New Article" (reverse-uri new-article))))
      (div
       ((class "content"))
       ,@content)))))

(define (render-not-found)
  (render-template
   '(h1 "Not Found")
   '(p "The page you are looking for does not exist.")))

(define (home req)
  (define all-articles (map render-article (unbox articles)))
  (render-template
   `(div ((class "articles")) ,@all-articles)))

(define (new-article req)
  (match (form-run article-form req)
    [(list 'passed article _)
     (article-add! article)
     (redirect-to (reverse-uri view-article (article-slug article)))]

    [(list _ _ render-widget)
     (render-template
      '(h1 "Add an Article")
      (render-article-form render-widget "Add Article"))]))

(define (view-article req slug)
  (define article (article-lookup slug))
  (cond
    [article (render-template (render-article article))]
    [else (render-not-found)]))

(define (edit-article req slug)
  (define article (article-lookup slug))
  (cond
    [article
     (match (form-run article-form req #:defaults (hash "author.name" (author-name (article-author article))
                                                        "author.email" (author-email (article-author article))
                                                        "title" (article-title article)
                                                        "content" (article-content article)) )
       [(list 'passed article _)
        (article-replace! slug article)
        (redirect-to (reverse-uri view-article (article-slug article)))]

       [(list _ _ render-widget)
        (render-template
         '(h1 "Edit Article")
         (render-article-form render-widget))])]

    [else (render-not-found)]))

(define-values (start reverse-uri)
  (dispatch-rules
   [("") home]
   [("articles" "new") #:method (or "get" "post") new-article]
   [("articles" (string-arg)) view-article]
   [("articles" (string-arg) "edit") #:method (or "get" "post") edit-article]))

(define ((csrf-protect handler) req . args)
  (define csrf-cookie-name "csrf_token")
  (define csrf-cookie (findf (lambda (cookie)
                               (string=? csrf-cookie-name (client-cookie-name cookie)))
                             (request-cookies req)))

  (define csrf-value
    (or (and csrf-cookie (client-cookie-value csrf-cookie))
        (bytes->string/utf-8 (base64-encode (crypto-random-bytes 128) #""))))

  (when (member (request-method req) '(#"DELETE" #"PATCH" #"POST" #"PUT"))
    (define csrf-binding (bindings-assq #"csrf_token" (request-bindings/raw req)))
    (when (not (and csrf-binding (string=? (bytes->string/utf-8 (binding:form-value csrf-binding)) csrf-value)))
      (error 'csrf-protect)))

  (define handler-response
    (parameterize ([csrf-token csrf-value])
      (apply handler req args)))

  (if csrf-cookie
      handler-response
      (struct-copy response handler-response [headers (cons
                                                       (cookie->header (make-cookie csrf-cookie-name csrf-value))
                                                       (response-headers handler-response))])))

(module+ main
  (serve/dispatch (csrf-protect start)))
