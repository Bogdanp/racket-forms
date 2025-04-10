#lang scribble/manual

@(require (for-label forms
                     json
                     racket
                     web-server/http
                     xml)
          racket/sandbox
          scribble/example)

@title{Forms: Web Form Validation}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]

@defmodule[forms]


@;; Introduction ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section[#:tag "intro"]{Introduction}

This library lets you declaratively validate web form data.  It
differs from the formlets provided by @secref["formlets" #:doc '(lib
"web-server/scribblings/web-server.scrbl")] in two important ways:

@itemlist[
  @item{validation and presentation are separate, and}
  @item{you are given the ability to display and control validation errors.}
]

@subsection[#:tag "tutorial"]{Tutorial}

@subsubsection[#:tag "validation"]{Validation}

@racket[form]s are composed of other forms and @tech{formlets}.  A
basic form might look like this:

@(define eval
   (call-with-trusted-sandbox-configuration
     (lambda ()
        (parameterize ([sandbox-output 'string]
                       [sandbox-error-output 'string]
                       [sandbox-memory-limit 50])
          (make-evaluator 'racket/base)))))

@(define-syntax-rule (demo e ...)
   (examples #:eval eval #:label #f e ...))

@examples[
  #:eval eval
  #:label #f
  #:no-prompt
  #:hidden
  (require forms/base
           racket/match
           racket/pretty
           racket/promise
           net/url
           web-server/http)
]

@demo[
  #:no-prompt
  (define simple-form
    (form
      (lambda (name)
        (and name (string-upcase name)))
      (list (cons 'name binding/text))))
]

This form accepts an optional text value named @racket["name"] and
returns its upper-cased version.  To validate some data against this
form we can call @racket[form-validate]:

@demo[
  (form-validate simple-form (hash))
  (form-validate simple-form (hash "name" (make-binding:form #"name" #"Bogdan")))
]

Formlets can be chained in order to generate more powerful
validations.  If we wanted the above form to require the
@racket["name"] field, we'd combine @racket[binding/text] with
@racket[required] using @racket[ensure]:

@demo[
  #:no-prompt
  (define simple-form
    (form
      (lambda (name)
        (string-upcase name))
      (list (cons 'name (ensure binding/text (required))))))
]

If we validate the same data against @racket[simple-form] now, our
results differ slightly:

@demo[
  (form-validate simple-form (hash))
  (form-validate simple-form (hash "name" (make-binding:form #"name" #"Bogdan")))
]

Notice how in the first example, an error was returned instead of
@racket[#f] and we no longer need to guard against false values in our
lambda.

So far so good, but the syntax used to declare these forms can get
unwieldy as soon as your forms grow larger than a couple fields.  The
library provides @racket[form*], which is a convenience macro designed
to make writing large forms more manageable.  In day-to-day use, you'd
declare the above form like this:

@demo[
  #:no-prompt
  (define simple-form
    (form* ([name (ensure binding/text (required))])
      (string-upcase name)))
]

If you're thinking "Hey, that looks like a @racket[let]"!  You're on
the right track.

@subsubsection[#:tag "Presentation"]{Presentation}

Let's take a slightly more complicated form:

@demo[
  #:no-prompt
  (define login-form
    (form* ([username (ensure binding/email (required) (shorter-than 150))]
            [password (ensure binding/text (required) (longer-than 8))])
      (list username password)))
]

This form expects a valid e-mail address and a password longer than 8
characters and it returns a list containing the two values on success.
To render this form to HTML, we can define a function that returns an
x-expression:

@demo[
  #:no-prompt
  (define (render-login-form)
    '(form
       ((action "")
        (method "POST"))
       (label
         "Username"
         (input ((type "email") (name "username"))))
       (label
         (input ((type "password") (name "password"))))
       (button ((type "submit")) "Login")))
]

This will do the trick, but it has two problems:

@itemlist[
  @item{
    if there are any validation errors and we re-display the form to the user, the
    previously-submitted values won't show up,
  }
  @item{
    nor will any validation errors.
  }
]

We can use @tech{widgets} to fix both problems.  First, we have to
update @racket[render-login-form] to take a @racket[widget-renderer/c]
as input:

@demo[
  #:no-prompt
  (define (render-login-form render-widget)
    '(form
       ((action "")
        (method "POST"))
       (label
         "Username"
         (input ((type "email") (name "username"))))
       (label
         "Password"
         (input ((type "password") (name "password"))))
       (button ((type "submit")) "Login")))
]

Second, instead of rendering the input fields ourselves, we can tell
@racket[render-widget] to render the appropriate widgets for those
fields:

@demo[
  #:no-prompt
  (define (render-login-form render-widget)
    `(form
       ((action "")
        (method "POST"))
       (label
         "Username"
         ,(render-widget "username" (widget-email)))
       (label
         "Password"
         ,(render-widget "password" (widget-password)))
       (button ((type "submit")) "Login")))
]

Finally, we can also begin rendering errors:

@demo[
  #:no-prompt
  (define (render-login-form render-widget)
    `(form
       ((action "")
        (method "POST"))
       (label
         "Username"
         ,(render-widget "username" (widget-email)))
       ,@(render-widget "username" (widget-errors))
       (label
         "Password"
         ,(render-widget "password" (widget-password)))
       ,@(render-widget "password" (widget-errors))
       (button ((type "submit")) "Login")))
]

To compose the validation and the presentation aspects, we can use
@racket[form-run]:

@demo[
  #:no-prompt
  (define (make-request #:method [method #"GET"]
                        #:url [url "http://example.com"]
                        #:headers [headers null]
                        #:bindings [bindings null])
    (request method (string->url url) headers (delay bindings) #f "127.0.0.1" 8000 "127.0.0.1"))
]

@demo[
  (pretty-print
   (form-run login-form (make-request)))
]

@racket[form-run] is smart enough to figure out whether or not the
request should be validated based on the request method.  Because we
gave it a @tt{GET} request above, it returned a @racket['pending]
result and a widget renderer.  That same renderer can be passed to our
@racket[render-login-form] function:

@demo[
  (match-define (list _ _ render-widget)
    (form-run login-form (make-request)))

  (pretty-print
   (render-login-form render-widget))
]

If we pass it an empty @tt{POST} request instead, the data will be
validated and a @racket['failed] result will be returned:

@demo[
  (pretty-print
   (form-run login-form (make-request #:method #"POST")))
]

Finally, if we pass it a valid @tt{POST} request, we'll get a
@racket['passed] result:

@demo[
  (define req
    (make-request
     #:method #"POST"
     #:bindings (list (make-binding:form #"username" #"bogdan@defn.io")
                      (make-binding:form #"password" #"hunter1234"))))
  (pretty-print
   (form-run login-form req))
]

Putting it all together, we might write a request handler that looks
like this:

@demo[
  #:no-prompt
  (define (login req)
    (match (form-run login-form req)
      [(list 'passed (list username password) _)
       (login-user! username password)
       (redirect-to "/dashboard")]

      [(list _ _ render-widget)
       (response/xexpr (render-login-form render-widget))]))
]

@subsubsection[#:tag "nesting"]{Nested Validation}

I left one thing out of the tutorial that you might be wondering
about.  Aside from plain values, @racket[form]s can also return
@racket[ok?] or @racket[err?] values.  This makes it possible to do
things like validate that two fields have the same value.

@demo[
  #:no-prompt
  (define signup-form
    (form* ([username (ensure binding/email (required) (shorter-than 150))]
            [password (form* ([p1 (ensure binding/text (required) (longer-than 8))]
                              [p2 (ensure binding/text (required) (longer-than 8))])
                        (cond
                          [(string=? p1 p2) (ok p1)]
                          [else (err '((p1 . "The passwords must match.")
                                       (p2 . "The passwords must match.")))]))])
      (list username password)))
]

This form will validate that the two password fields contain the same
value and then return the first of them.  When rendering the subform,
you'd use @racket[widget-namespace] to produce a widget renderer for
the nested form's fields.

@subsubsection[#:tag "next-steps"]{Next Steps}

@(define examples-url "https://github.com/Bogdanp/racket-forms/tree/master/examples")

If the tutorial left you wanting for more, take a look at the
reference documentation below and also check out the
@link[examples-url]{examples} folder in the source code repository.


@;; Reference ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section[#:tag "reference"]{Reference}

@subsection[#:tag "forms"]{Forms}

@defstruct[form ([constructor any/c]
                 [children (listof (cons/c symbol? (or (cons/c (or/c 'ok 'err) any/c) form?)))])]{

  Forms are composed of a list of @tech{formlets} and other forms.

  Upon successful validation, the results of each of the
  @racket[children] are passed in order to the @racket[constructor]
  and an @racket[ok] value is returned.

  On failure, an @racket[err] value is returned containing a list of
  errors for every child that failed validation.
}

@defform[(form* ([name formlet] ...+)
           e ...+)]{
  Syntactic sugar for defining @racket[form]s.
}

@defproc[(form-validate [form form?]
                        [bindings (hash/c string? any/c)]) (cons/c (or/c 'ok 'err) any/c)]{
  Validate @racket[bindings] against @racket[form].
}

@defproc[(form-run [f form?]
                   [r request?]
                   [#:combine combine-proc (-> any/c any/c any/c any/c) (lambda (k v1 v2) v2)]
                   [#:defaults defaults (hash/c string? binding?) (hash)]
                   [#:submit-methods submit-methods (listof bytes?) '(#"DELETE" #"PATCH" #"POST" #"PUT")])
         (or/c
          (list/c 'passed any/c widget-renderer/c)
          (list/c 'failed any/c widget-renderer/c)
          (list/c 'pending #f widget-renderer/c))]{

  Validates @racket[r] against @racket[f]. The @racket[#:combine]
  argument controls how multiple bindings for the same field are
  handled.

  For example, to combine all the bindings for a given field for use
  with @racket[binding/list], you would pass the following procedure as
  the @racket[#:combine] argument:

  @racketblock[
    (lambda (_k v1 v2)
      (if (pair? v1)
          (append v1 (list v2))
          (list v1 v2)))
  ]

  @history[#:changed "0.6" @elem{Added the @racket[#:combine]
    argument.}]
}

@subsection[#:tag "formlets"]{Formlets}

@deftech{Formlets} extract, validate and transform field values from
forms.

@defthing[binding/file (-> (or/c #f binding:file?)
                           (or/c (cons/c 'ok (or/c #f binding:file?))
                                 (cons/c 'err string?)))]{
  Extracts an optional @racket[binding:file].
}

@defthing[binding/text (-> (or/c #f binding:form?)
                           (or/c (cons/c 'ok (or/c #f string?))
                                 (cons/c 'err string?)))]{
  Converts an optional @racket[binding:form] to a @racket[string?].
}

@defthing[binding/boolean (-> (or/c #f binding:form?)
                              (or/c (cons/c 'ok (or/c #f boolean?))
                                    (cons/c 'err string?)))]{
  Converts an optional @racket[binding:form] to a @racket[boolean?].
}

@defthing[binding/email (-> (or/c #f binding:form?)
                            (or/c (cons/c 'ok (or/c #f string?))
                                  (cons/c 'err string?)))]{
  Converts an optional @racket[binding:form] to a @racket[string?],
  ensuring that it contains something vaguely resembling an e-mail
  address.
}

@defthing[binding/number (-> (or/c #f binding:form?)
                             (or/c (cons/c 'ok (or/c #f number?))
                                   (cons/c 'err string?)))]{
  Converts an optional @racket[binding:form] to a @racket[number?].
}

@defthing[binding/symbol (-> (or/c #f binding:form?)
                             (or/c (cons/c 'ok (or/c #f symbol?))
                                   (cons/c 'err string?)))]{
  Converts an optional @racket[binding:form] to a @racket[symbol?].
}

@defthing[binding/json (-> (or/c #f binding:form?)
                           (or/c (cons/c 'ok (or/c #f jsexpr?))
                                 (cons/c 'err string?)))]{
  Converts an optional @racket[binding:form] to a @racket[jsexpr?].

  @history[#:added "0.7"]
}

@defthing[binding/list (-> (or/c #f binding:form? (listof binding:form?))
                           (or/c (cons/c 'ok (or/c #f (listof (or/c #f string?)))
                                 (cons/c 'err (listof string?)))))] {

  Converts an optional list of @racket[binding:form]s to a list of
  optional strings. Use this when you want to accumulate values from
  fields with the same name into a list. To use it, you must provide a
  @racket[#:combine] procedure to @racket[form-run].

  @history[#:added "0.8"]
}

@subsubsection[#:tag "primitives"]{Primitives}

These functions produce formlets either by combining other formlets or
by "lifting" normal values into the formlet space.

@deftogether[
  (@defproc[(ok [x any/c]) (cons/c 'ok any/c)]
   @defproc[(ok? [x any/c]) boolean?])]{
  Creates a formlet that always returns @racket[x].
}

@deftogether[
  (@defproc[(err [x any/c]) (cons/c 'err any/c)]
   @defproc[(err? [x any/c]) boolean?])]{
  Creates an errored formlet.
}

@defproc[(ensure [f (-> any/c (or/c (cons/c 'ok any/c)
                                    (cons/c 'err string?)))] ...+) (-> any/c (or/c (cons/c 'ok any/c)
                                                                                   (cons/c 'err string?)))]{
  Sequences two or more formlets together, producing a formlet that
  short-circuits on the first error.
}

@subsubsection[#:tag "validators"]{Validators}

These functions produce basic validator formlets.

@defproc[(required [#:message message string? "This field is required."])
         (-> (or/c #f any/c)
             (or/c (cons/c 'ok string?)
                   (cons/c 'err string?)))]{
  Ensures that a value is present. Must be run after one of the
  @tt{binding/} formlets.
}

@defproc[(matches [pattern regexp?]
                  [#:message message string? (format "This field must match the regular expression ~v." p)])
         (-> (or/c string? #f)
             (or/c (cons/c 'ok string?)
                   (cons/c 'err string?)))]{
  Ensures that an optional @racket[string?] matches the given @racket[pattern].
}

@defproc[(one-of [pairs (listof (cons/c any/c any/c))]
                 [#:message message string? (format "This field must contain one of the following values: ~a" (string-join (map car pairs) ", "))])
         (-> (or/c any/c #f)
             (or/c (cons/c 'ok any/c)
                   (cons/c 'err string?)))]{
  Ensures that an optional @racket[string?] is equal to one of the
  @racket[car]s of the provided list of pairs, producing the
  @racket[cdr] of the matched pair.
}

@defproc[(shorter-than [n exact-positive-integer?]
                       [#:message message string? (format "This field must contain ~a or fewer characters." (sub1 n))])
         (-> (or/c string? #f)
             (or/c (cons/c 'ok string?)
                   (cons/c 'err string?)))]{
  Ensures that an optional @racket[string?] is shorter than @racket[n].
}

@defproc[(longer-than [n exact-positive-integer?]
                      [#:message message string? (format "This field must contain ~a or more characters." (add1 n))])
         (-> (or/c string? #f)
             (or/c (cons/c 'ok string?)
                   (cons/c 'err string?)))]{
  Ensures that an optional @racket[string?] is longer than @racket[n].
}

@defproc[(to-integer [#:message message string? "This field must contain an integer."])
         (-> (or/c number? #f)
             (or/c (cons/c 'ok real?)
                   (cons/c 'err string?)))]{
  Ensures that an optional @racket[number?] is an @racket[integer?].

  @history[#:added "0.6.1"]
}

@defproc[(to-real [#:message message string? "This field must contain a real number."])
         (-> (or/c number? #f)
             (or/c (cons/c 'ok real?)
                   (cons/c 'err string?)))]{
  Ensures that an optional @racket[number?] is a @racket[real?] number.
}

@defproc[(range/inclusive [min real?]
                          [max real?]
                          [#:message message string? (format "This field must contain a number that lies between ~a and ~a, inclusive." (~r min) (~r max))])
         (-> (or/c real? #f)
             (or/c (cons/c 'ok real?)
                   (cons/c 'err string?)))]{
  Ensures that an optional @racket[real?] lies between @racket[min] and @racket[max], inclusive.
}

@defproc[(list-of [formlet (-> (or/c #f binding:form?)
                               (or/c (cons/c 'ok any/c)
                                     (cons/c 'err string?)))])
         (-> (listof (or/c #f string?))
             (or/c (cons/c 'ok (listof (or/c #f string?)))
                   (cons/c 'err (listof string?))))]{

  Ensures that every element of a @racket[binding/list] matches the
  given @racket[formlet]. When one of the elements of the list fails
  validation, the entire list is marked failed with valid elements
  having @racket[""] as their error.

  @examples[
  #:eval eval
  #:label #f
  ((list-of binding/number) (list "123" ""))
  ((list-of (ensure binding/number (required))) (list "123" ""))
  ]

  @history[#:added "0.8"]
}

@defproc[(list-of-length [n exact-nonnegative-integer?]
                         [#:too-few-elements-message too-few-message string? "This field is required."]
                         [#:too-many-elements-message too-many-message string? "This field should not exist."])
         (-> (listof (or/c #f string?))
             (or/c (cons/c 'ok (listof (or/c #f string?)))
                   (cons/c 'err (listof string?))))]{

  Ensures that the result of a @racket[binding/list] has exactly
  @racket[n] elements.

  @examples[
  #:eval eval
  #:label #f
  ((list-of-length 3) (list "123" ""))
  ((list-of-length 3) (list "123" "" "abc"))
  ((list-of-length 3) (list "123" "" "abc" "foo"))
  ]

  @history[#:added "0.8"]
}

@subsection[#:tag "widgets"]{Widgets}

@deftech{Widgets} render fields into @racket[xexpr?]s.

@defproc[(widget-input [#:type type string?]
                       [#:omit-value? omit-value? boolean? #f]
                       [#:attributes attributes attributes/c null]) widget/c]{
  Returns a widget that can render an @tt{<input>} element.
}

@defproc[(widget-errors [#:class class string?]) widget/c]{
  Returns a widget that can render errors.

  @demo[
    ((widget-errors) "example" #f null)
    ((widget-errors) "example" #f '((example . "this field is required")
                                    (another-field . "this field is required")))
  ]
}

@defproc[(widget-checkbox [#:attributes attributes attributes/c null]) widget/c]{
  Returns a widget that can render an @tt{<input type="checkbox">} element.

  @demo[
    ((widget-checkbox) "example" #f null)
    ((widget-checkbox) "example" (binding:form #"" #"value") null)
  ]
}

@defproc[(widget-email [#:attributes attributes attributes/c null]) widget/c]{
  Returns a widget that can render an @tt{<input type="email">} element.

  @demo[
    ((widget-email) "example" #f null)
    ((widget-email) "example" (binding:form #"" #"value@example.com") null)
  ]
}

@defproc[(widget-file [#:attributes attributes attributes/c null]) widget/c]{
  Returns a widget that can render an @tt{<input type="file">} element.

  @demo[
    ((widget-file) "example" #f null)
    ((widget-file) "example" (binding:file #"" #"filename" null #"content") null)
  ]
}

@defproc[(widget-hidden [#:attributes attributes attributes/c null]) widget/c]{
  Returns a widget that can render an @tt{<input type="hidden">} element.

  @demo[
    ((widget-hidden) "example" #f null)
    ((widget-hidden) "example" (binding:form #"" #"value") null)
  ]
}

@defproc[(widget-number [#:attributes attributes attributes/c null]) widget/c]{
  Returns a widget that can render an @tt{<input type="number">} element.

  @demo[
    ((widget-number) "example" #f null)
    ((widget-number) "example" (binding:form #"" #"1") null)
  ]
}

@defproc[(widget-password [#:attributes attributes attributes/c null]) widget/c]{
  Returns a widget that can render an @tt{<input type="password">} element.

  @demo[
    ((widget-password) "example" #f null)
    ((widget-password) "example" (binding:form #"" #"value") null)
  ]
}

@defproc[(widget-select [options select-options/c]
                        [#:attributes attributes attributes/c null]) widget/c]{
  Returns a widget that can render a @tt{<select>} element.

  @demo[
    (define sel
      (widget-select '(("value-a" . "Label A")
                       ("Countries" (("romania" . "Romania")
                                     ("usa" . "United States of America")))
                       ("Languages" (("english" . "English")
                                     ("racket" . "Racket"))))))
    (pretty-print (sel "example" #f null))
    (pretty-print (sel "example" (binding:form #"" #"racket") null))
  ]
}

@defproc[(widget-radio-group [options radio-options/c]
                             [#:attributes attributes attributes/c null]) widget/c]{
  Returns a widget that can render a group of @tt{<input type="radio">} elements.

  @demo[
    (define rg
     (widget-radio-group '(("value-a" . "Label A")
                           ("value-b" . "Label B"))))
    (pretty-print (rg "example" #f null))
    (pretty-print (rg "example" (binding:form #"" #"value-a") null))
  ]
}

@defproc[(widget-text [#:attributes attributes attributes/c null]) widget/c]{
  Returns a widget that can render an @tt{<input type="text">} element.

  @demo[
    ((widget-text) "example" #f null)
    ((widget-text) "example" (binding:form #"" #"value") null)
  ]
}

@defproc[(widget-textarea [#:attributes attributes attributes/c null]) widget/c]{
  Returns a widget that can render a @tt{<textarea>} element.

  @demo[
    ((widget-textarea) "example" #f null)
    ((widget-textarea) "example" (binding:form #"" #"value") null)
  ]
}

@defproc[(widget-namespace [namespace string?]
                           [widget-renderer widget-renderer/c]) widget/c]{
  Returns a widget renderer for the subform whose id is
  @racket[namespace].
}

@defproc[(widget-list [render-proc (-> (->* [widget/c]
                                            [exact-nonnegative-integer?]
                                            (or/c xexpr/c (listof xexpr/c)))
                                       widget/c)]) widget/c]{
  Returns a widget that calls @racket[render-proc] with a procedure
  that, given a widget, renders that widget for the n-th element of a
  list, where every invocation of the the element rendering procedure
  increments the element index.

  @examples[
  #:eval eval
  #:label #f
  #:no-prompt
  (define the-widget
    (widget-list
     (lambda (render-element)
       `(div
         (label "Element 1:" ,(render-element (widget-number)))
         ,@(render-element (widget-errors) 0)
         (label "Element 2:" ,(render-element (widget-text)))
         ,@(render-element (widget-errors) 1)))))
  (pretty-print
   (the-widget
    "example"
    (list
     (binding:form #"example" #"")
     (binding:form #"example" #""))
    null))
  ]

  @history[#:added "0.8"]
}

@subsubsection{Contracts}

@defthing[attributes/c (listof (list/c symbol? string?))]{
  The contract for element attributes.
}

@defthing[errors/c (listof (or/c string? (cons/c symbol? (or/c string? errors/c))))]{
  The contract for lists of validation errors.
}

@defthing[radio-options/c (listof (cons/c string? string?))]{
  The contract for a list of @tt{<input type="radio">} options.
}

@defthing[select-options/c (listof
                            (or/c (cons/c string? string?)
                                  (list/c string? (listof (cons/c string? string?)))))]{
  The contract for a list of @tt{<select>} element options.  The
  list-based variant is used to represent option groups
  (@tt{<optgroup> elements}).
}

@defthing[widget/c (-> string? (or/c #f binding?) errors/c (or/c xexpr/c (listof xexpr/c)))]{
  The contract for @tech{widgets}.  Widgets take a field name, its
  associated binding (if any) and a list of errors and produces either
  one or a list of @racket[xexpr?]s.
}

@defthing[widget-renderer/c (-> string? widget/c (or/c xexpr/c (listof xexpr/c)))]{
  The contract for widget renderers.  Renderers take the name of a
  field and a @tech{widget} that will render the field as one or more
  xexpressions.
}
