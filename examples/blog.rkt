#lang racket/base

(require forms/base
         racket/function
         racket/match
         web-server/dispatch
         web-server/http
         web-server/servlet/web)

(struct author (name email))
(struct article (author title slug content))

(define (slugify title)
  (regexp-replace* #px"[^a-zA-Z0-9\\-_]" (string-downcase title) "-"))

(define author-form
  (form* ([name (ensure binding/text (required) (longer-than 3) (shorter-than 50))]
          [email (ensure binding/email (required))])
    (author name email)))

(define article-form
  (form* ([author author-form]
          [title (ensure binding/text (required) (longer-than 1) (shorter-than 150))]
          [content (ensure binding/text (required) (longer-than 1))]
          [_agreement (ensure binding/text (required #:message "You must check this box."))])
    (article author title (slugify title) content)))

(define *articles*
  (box (list (article (author "Bogdan Popa" "bogdan@defn.io") "Part Two" "part-two" "Hi, it's me again!")
             (article (author "Bogdan Popa" "bogdan@defn.io") "Hello World!" "hello-world-" "Hi there, world!"))))

(define (find-article slug)
  (for/first ([article (unbox *articles*)]
              #:when (string=? (article-slug article) slug))
    article))

(define (add-article! new-article)
  (box-swap! *articles* (lambda (articles)
                          (cons new-article articles))))

(define (replace-article! slug new-article)
  (box-swap! *articles* (lambda (articles)
                          (for/list ([article articles])
                            (if (string=? (article-slug article) slug)
                                new-article
                                article)))))

(define (box-swap! b f . args)
  (let loop ([v (unbox b)])
    (unless (box-cas! b v (apply f v args))
      (loop (unbox b)))))

(define ((labeled label [w (widget-text)]) name value errors)
  `(div
    ([class "form__group"])
    (label ,label " " ,(w name value errors))
    ,@((widget-errors #:class "form__errors") name value errors)))

(define (render-author-form render-widget)
  `(fieldset
    ([class "form__fieldset"])
    (legend "Author")
    ,(render-widget "name" (labeled "Name"))
    ,(render-widget "email" (labeled "Email" (widget-email)))))

(define (render-article-form target render-widget [action-label "Save"])
  `(form
    ([action ,target]
     [method "POST"]
     [class "form"])
    ,(render-author-form (widget-namespace "author" render-widget))
    (fieldset
     ([class "form__fieldset"])
     (legend "Article")
     ,(render-widget "title" (labeled "Title"))
     ,(render-widget "content" (labeled "Content" (widget-textarea #:attributes '([columns "140"] [rows "5"]))))
     ,(render-widget "_agreement" (labeled "I solemnly swear that I am up to no good" (widget-checkbox))))
    (button ([type "submit"]) ,action-label)))

(define (render-article the-article)
  `(div
    ([class "article"])
    (h2
     ([class "article__title"])
     (a
      ([href ,(reverse-uri view-article (article-slug the-article))])
      ,(article-title the-article)))
    (h5
     ([class "article__subtitle"])
     "Posted by " ,(author-name (article-author the-article)) " "
     (a ([href ,(reverse-uri edit-article (article-slug the-article))]) "Edit"))
    (div
     ([class "article__content"])
     ,(article-content the-article))))

(define (render-template . content)
  (define (render-nav-item label uri)
    `(li
      ([class "nav__items"])
      (a ((href ,uri)) ,label)))

  (response/xexpr
   #:preamble #"<!DOCTYPE html>"
   `(html
     (head
      (title "Blag"))
     (body
      (div
       ([class "nav"])
       (ul
        ([class "nav__items"])
        ,(render-nav-item "Home" (reverse-uri home))
        ,(render-nav-item "New Article" (reverse-uri new-article))))
      (div
       ([class "content"])
       ,@content)))))

(define (render-not-found)
  (render-template
   '(h1 "Not Found")
   '(p "The page you are looking for does not exist.")))

(define (home _req)
  (define all-articles (map render-article (unbox *articles*)))
  (render-template
   `(div ([class "articles"]) ,@all-articles)))

(define (new-article req)
  (send/suspend/dispatch
   (lambda (embed/url)
     (match (form-run article-form req)
       [(list 'passed article _)
        (add-article! article)
        (redirect-to (reverse-uri view-article (article-slug article)))]

       [(list _ _ render-widget)
        (render-template
         '(h1 "Add an Article")
         (render-article-form (embed/url new-article) render-widget "Add Article"))]))))

(define (view-article _req slug)
  (cond
    [(find-article slug)
     => (lambda (the-article)
          (render-template (render-article the-article)))]
    [else
     (render-not-found)]))

(define (edit-article req slug)
  (send/suspend/dispatch
   (lambda (embed/url)
     (cond
       [(find-article slug)
        => (match-lambda
             [(article author title slug content)
              (define defaults
                (hash "author.name" (author-name author)
                      "author.email" (author-email author)
                      "title" title
                      "content" content))

              (match (form-run article-form req #:defaults defaults)
                [(list 'passed new-article _)
                 (replace-article! slug new-article)
                 (redirect-to (reverse-uri edit-article slug))]

                [(list _ _ render-widget)
                 (render-template
                  '(h1 "Edit Article")
                  (render-article-form (embed/url (curryr edit-article slug)) render-widget))])])]

       [else
        (render-not-found)]))))

(define-values (start reverse-uri)
  (dispatch-rules
   [("") home]
   [("articles" "new") new-article]
   [("articles" (string-arg)) view-article]
   [("articles" (string-arg) "edit") edit-article]))

(module+ main
  (serve/dispatch start))
