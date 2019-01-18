#lang racket

(require forms/base
         net/base64
         racket/function
         racket/random
         web-server/dispatch
         web-server/http
         web-server/servlet/web)

(define (slugify title)
  (regexp-replace* #px"[^a-zA-Z0-9\\-_]" (string-downcase title) "-"))

(struct author (name email))

(define author-form
  (form* ([name (ensure text (required) (longer-than 3) (shorter-than 50))]
          [email (ensure email (required))])
    (author name email)))

(struct article (author title slug content))

(define article-form
  (form* ([author author-form]
          [title (ensure text (required) (longer-than 1) (shorter-than 150))]
          [content (ensure text (required) (longer-than 1))])
    (article author title (slugify title) content)))

(define articles (box (list (article (author "Bogdan Popa" "bogdan@defn.io")
                                     "Part Two"
                                     "part-two"
                                     "Hi, it's me again!")
                            (article (author "Bogdan Popa" "bogdan@defn.io")
                                     "Hello World!"
                                     "hello-world-"
                                     "Hi there, world!"))))

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

(define (render-article-form target render-widget [action-label "Save"])
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
    ((action ,target)
     (method "POST")
     (class "form"))
    ,author-form
    ,content-form
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

(define (redirect/forget uri)
  (send/forward (lambda _ (redirect-to uri))))

(define (home req)
  (define all-articles (map render-article (unbox articles)))
  (render-template
   `(div ((class "articles")) ,@all-articles)))

(define (new-article req)
  (send/suspend/dispatch
   (lambda (embed/url)
     (match (form-run article-form req)
       [(list 'passed article _)
        (article-add! article)
        (redirect/forget (reverse-uri view-article (article-slug article)))]

       [(list _ _ render-widget)
        (render-template
         '(h1 "Add an Article")
         (render-article-form (embed/url new-article) render-widget "Add Article"))]))))

(define (view-article req slug)
  (define article (article-lookup slug))
  (cond
    [article (render-template (render-article article))]
    [else (render-not-found)]))

(define (edit-article req slug)
  (send/suspend/dispatch
   (lambda (embed/url)
     (define article (article-lookup slug))
     (cond
       [article
        (define defaults
          (hash "author.name" (author-name (article-author article))
                "author.email" (author-email (article-author article))
                "title" (article-title article)
                "content" (article-content article)))

        (match (form-run article-form req #:defaults defaults)
          [(list 'passed article _)
           (article-replace! slug article)
           (redirect/forget (reverse-uri edit-article (article-slug article)))]

          [(list _ _ render-widget)
           (render-template
            '(h1 "Edit Article")
            (render-article-form (embed/url (curryr edit-article slug)) render-widget))])]

       [else (render-not-found)]))))

(define-values (start reverse-uri)
  (dispatch-rules
   [("") home]
   [("articles" "new") new-article]
   [("articles" (string-arg)) view-article]
   [("articles" (string-arg) "edit") edit-article]))

(module+ main
  (serve/dispatch start))
