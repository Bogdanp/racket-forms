#lang scribble/manual

@(require (for-label forms
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

This library lets you declaratively validate web form data (and
potentially json, xml and other structured data).  It differs from the
formlets provided by @secref["formlets" #:doc '(lib "web-server/scribblings/web-server.scrbl")]
in two important ways:

@itemlist[
  @item{the validation model and the presentation are separate, and}
  @item{you are given the ability to display and control validation errors.}
]

@subsection[#:tag "tutorial"]{Tutorial}

@subsubsection[#:tag "validation"]{Validation}

@racket[form]s are composed of other forms and formlets, functions
that can take an input, validate it and potentially transform it into
another value.

A basic form might look like this:

@(define eval
   (call-with-trusted-sandbox-configuration
     (lambda ()
        (parameterize ([sandbox-output 'string]
                       [sandbox-error-output 'string]
                       [sandbox-memory-limit 50])
          (make-evaluator 'racket/base)))))

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

@examples[
  #:eval eval
  #:label #f
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

@examples[
  #:eval eval
  #:label #f
  (form-validate simple-form (hash))
  (form-validate simple-form (hash "name" (make-binding:form #"name" #"Bogdan")))
]

Formlets can be chained in order to generate more powerful
validations.  If we wanted the above form to require the
@racket["name"] field, we'd combine @racket[binding/text] with
@racket[required] using @racket[ensure]:

@examples[
  #:eval eval
  #:label #f
  #:no-prompt
  (define simple-form
    (form
      (lambda (name)
        (string-upcase name))
      (list (cons 'name (ensure binding/text (required))))))
]

If we validate the same data against @racket[simple-form] now, our
results differ slightly:

@examples[
  #:eval eval
  #:label #f
  (form-validate simple-form (hash))
  (form-validate simple-form (hash "name" (make-binding:form #"name" #"Bogdan")))
]

Notice how in the first example, an error was returned instead of
@racket[#f] and we no longer need to guard against false values in our
lambda.

So far so good, but the syntax used to declare these forms can get
unwieldy as soon as your forms grow larger than a couple fields.  The
library provides @racket[form*], which is a convenience macro to make
writing large forms more manageable.  In day-to-day use, you'd declare
the above form like this:

@examples[
  #:eval eval
  #:label #f
  #:no-prompt
  (define simple-form
    (form* ([name (ensure binding/text (required))])
      (string-upcase name)))
]

If you're thinking "Hey, that looks like a @racket[let]"!  You're on
the right track.

@subsubsection[#:tag "Presentation"]{Presentation}

Let's take a slightly more complicated form:

@examples[
  #:eval eval
  #:label #f
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

@examples[
  #:eval eval
  #:label #f
  #:no-prompt
  (define (render-login-form)
    '(form
       ((action ""))
       ((method "POST"))
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

We can use "widgets" to fix both problems.  First, we have to update
@racket[render-login-form] to take a widget rendering function as
input:

@examples[
  #:eval eval
  #:label #f
  #:no-prompt
  (define (render-login-form render-widget)
    '(form
       ((action ""))
       ((method "POST"))
       (label
         "Username"
         (input ((type "email") (name "username"))))
       (label
         "Password"
         (input ((type "password") (name "password"))))
       (button ((type "submit")) "Login")))
]

Second, instead of rendering the input fields ourselves, we can tell
render-widget to render the appropriate widgets for those fields:

@examples[
  #:eval eval
  #:label #f
  #:no-prompt
  (define (render-login-form render-widget)
    `(form
       ((action ""))
       ((method "POST"))
       (label
         "Username"
         ,(render-widget "username" (widget-email)))
       (label
         "Password"
         ,(render-widget "password" (widget-password)))
       (button ((type "submit")) "Login")))
]

Finally, we can also begin rendering errors:

@examples[
  #:eval eval
  #:label #f
  #:no-prompt
  (define (render-login-form render-widget)
    `(form
       ((action ""))
       ((method "POST"))
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

@examples[
  #:eval eval
  #:label #f
  #:no-prompt
  (define (make-request #:method [method #"GET"]
                        #:url [url "http://example.com"]
                        #:headers [headers null]
                        #:bindings [bindings null])
    (request method (string->url url) headers (delay bindings) #f "127.0.0.1" 8000 "127.0.0.1"))
]

@examples[
  #:eval eval
  #:label #f
  (form-run login-form (make-request))
]

@racket[form-run] is smart enough to figure out whether or not the
request should be validated based on the request method.  Because we
passed it a (fake) GET request above, it returned a @racket['pending]
result and a widget renderer.  That same renderer can be passed to our
@racket[render-login-form] function:

@examples[
  #:eval eval
  #:label #f
  (match-define (list _ _ render-widget)
    (form-run login-form (make-request)))

  (pretty-print (render-login-form render-widget))
]

If we pass it an empty POST request instead, the data will be
validated and a @racket['failed] result will be returned:

@examples[
  #:eval eval
  #:label #f
  (form-run login-form (make-request #:method #"POST"))
]

Finally, if we pass it a valid POST request, we'll get a
@racket['passed] result:

@examples[
  #:eval eval
  #:label #f
  (form-run login-form (make-request #:method #"POST"
                                     #:bindings (list (make-binding:form #"username" #"bogdan@defn.io")
                                                      (make-binding:form #"password" #"hunter1234"))))
]

Putting it all together, we might write a request handler that looks
like this:

@examples[
  #:eval eval
  #:label #f
  #:no-prompt
  (define (login req)
    (match (form-run req)
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

@examples[
  #:eval eval
  #:label #f
  #:no-prompt
  (define signup-form
    (form* ([username (ensure binding/email (required) (shorter-than 150))]
            [password (form* ([p1 (ensure binding/text (required) (longer-than 8))]
                              [p2 (ensure binding/text (required) (longer-than 8))])
                        (cond
                          [(string=? p1 p2) (ok p1)]
                          [else (err "The passwords must match.")]))])
      (list username password)))
]

This form will validate that the two password fields contain the same
value and then return the first value.

@subsubsection[#:tag "next-steps"]{Next Steps}

If the tutorial left you wanting for more, take a look at the
reference documentation below and also check out the
@link["https://github.com/Bogdanp/racket-forms/tree/master/examples"]{examples}
folder in the source code repository.


@subsection[#:tag "limitations"]{Limitations}

The following features are not currently supported (and may never be):

@itemlist[
  @item{
    multi-valued form bindings
  }

  @item{
    dynamic lists of fields and forms -- it is possible to
    dynamically manipulate forms, but there is no nice syntax for it
  }

  @item{
    default form values must be passed to @racket[form-run] and cannot
    be encoded into the forms themselves -- this is inconvenient for
    some use cases but it hugely simplifies the implementation and it
    makes it so that the same form can be used, for example, to create
    something and also to edit that thing
  }
]


@;; Reference ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section[#:tag "reference"]{Reference}

@subsection[#:tag "formlets"]{Formlets}

@defthing[binding/file (-> (or/c false/c binding:file?)
                           (or/c (cons/c 'ok (or/c false/c binding:file?))
                                 (cons/c 'err string?)))]{
  Extracts an optional @racket[binding:file].
}

@defthing[binding/text (-> (or/c false/c binding:form?)
                           (or/c (cons/c 'ok (or/c false/c string?))
                                 (cons/c 'err string?)))]{
  Converts an optional @racket[binding:form] to a @racket[string?].
}

@defthing[binding/boolean (-> (or/c false/c binding:form?)
                              (or/c (cons/c 'ok (or/c false/c boolean?))
                                    (cons/c 'err string?)))]{
  Converts an optional @racket[binding:form] to a @racket[boolean?].
}

@defthing[binding/email (-> (or/c false/c binding:form?)
                            (or/c (cons/c 'ok (or/c false/c string?))
                                  (cons/c 'err string?)))]{
  Converts an optional @racket[binding:form] to a @racket[string?],
  ensuring that it contains something vaguely resembling an e-mail
  address.
}

@defthing[binding/number (-> (or/c false/c binding:form?)
                             (or/c (cons/c 'ok (or/c false/c number?))
                                   (cons/c 'err string?)))]{
  Converts an optional @racket[binding:form] to a @racket[number?].
}

@defthing[binding/symbol (-> (or/c false/c binding:form?)
                             (or/c (cons/c 'ok (or/c false/c symbol?))
                                   (cons/c 'err string?)))]{
  Converts an optional @racket[binding:form] to a @racket[symbol?].
}

@subsubsection[#:tag "primitives"]{Primitives}

These functions produce formlets either by combining other formlets or
by "lifting" normal values into the formlet space.

@deftogether[
  (@defproc[(ok [x any/c]) (cons/c 'ok any/c)]
   @defproc[(ok? [x any/c]) boolean?])]{
  Create a formlet that always returns @racket[x].
}

@deftogether[
  (@defproc[(err [x any/c]) (cons/c 'err any/c)]
   @defproc[(err? [x any/c]) boolean?])]{
  Create an errored formlet.
}

@defproc[(ensure [f (-> any/c (or/c (cons/c 'ok any/c)
                                    (cons/c 'err string?)))] ...+) (-> any/c (or/c (cons/c 'ok any/c)
                                                                                   (cons/c 'err string?)))]{
  Sequence two or more formlets together, producing a formlet that
  short-circuits on the first error.
}

@subsubsection[#:tag "validators"]{Validators}

These functions produce basic validator formlets.

@defproc[(required [#:message message string? "This field is required."])
         (-> (or/c false/c any/c)
             (or/c (cons/c 'ok string?)
                   (cons/c 'err string?)))]{
  Ensures that a non-empty @racket[string?] value is present.
}

@defproc[(matches [pattern regexp?]
                  [#:message message string? (format "This field must match the regular expression ~v." p)])
         (-> (or/c (false/c string?))
             (or/c (cons/c 'ok string?)
                   (cons/c 'err string?)))]{
  Ensures that an optional @racket[string?] matches the given @racket[pattern].
}

@defproc[(one-of [pairs (listof (cons/c any/c any/c))]
                 [#:message message string? (format "This field must contain one of the following values: ~a" (string-join (map car pairs) ", "))])
         (-> (or/c (false/c any/c))
             (or/c (cons/c 'ok any/c)
                   (cons/c 'err string?)))]{
  Ensures that an optional @racket[string?] is equal to one of the
  @racket[car]s of the provided list of pairs, producing the
  @racket[cdr] of the matched pair.
}

@defproc[(shorter-than [n exact-positive-integer?]
                       [#:message message string? (format "This field must contain ~a or fewer characters." (sub1 n))])
         (-> (or/c (false/c string?))
             (or/c (cons/c 'ok string?)
                   (cons/c 'err string?)))]{
  Ensures that an optional @racket[string?] is shorter than @racket[n].
}

@defproc[(longer-than [n exact-positive-integer?]
                      [#:message message string? (format "This field must contain ~a or more characters." (add1 n))])
         (-> (or/c (false/c string?))
             (or/c (cons/c 'ok string?)
                   (cons/c 'err string?)))]{
  Ensures that an optional @racket[string?] is longer than @racket[n].
}

@subsection[#:tag "forms"]{Forms}

@defstruct[form ([constructor any/c]
                 [children (listof (cons/c symbol? (or (cons/c (or/c 'ok 'err) any/c) form?)))])]{
  A form that can be used to validate @racket[children] together and
  produce a result value by passing the results of each field to
  @racket[constructor].
}

@defform[(form* ([name formlet] ...+)
           e ...+)]{
  Syntactic sugar for defining @racket[form]s.
}

@defproc[(form-validate [form form?]
                        [bindings (hash/c string? any/c)]) (cons/c (or/c 'ok 'err) any/c)]{
  Validate @racket[bindings] against @racket[form].
}

@defproc[(form-run [form form?]
                   [request request?]
                   [#:defaults defaults (hash/c string? binding?) (hash)]
                   [#:submit-methods submit-methods (listof bytes?) '(#"DELETE" #"PATCH" #"POST" #"PUT")])
         (or/c
          (list/c 'passed any/c widget-renderer/c)
          (list/c 'failed any/c widget-renderer/c)
          (list/c 'pending false/c widget-renderer/c))]{
  Validate @racket[request] against @racket[form].
}

@subsection[#:tag "widgets"]{Widgets}

@deftogether[
  (@defthing[attributes/c (listof (list/c symbol? string?))]
   @defthing[errors/c (listof
                        (or/c string? (cons/c symbol? (or/c string? errors/c))))]
   @defthing[options/c (listof (cons/c string? string?))]
   @defthing[widget/c (-> string? (or/c false/c binding?) errors/c (or/c xexpr/c (listof xexpr/c)))]
   @defthing[widget-renderer/c (-> string? widget/c (or/c xexpr/c (listof xexpr/c)))])]{
  Various widget-related contracts.
}

@defproc[(widget-namespace [namespace string?]
                           [widget-renderer widget-renderer/c]) widget/c]{
  Produce a widget renderer for a subform.
}

@defproc[(widget-errors [#:class class string?]) widget/c]{
  Produce a widget that can render errors.
}

@defproc[(widget-input [#:type type string?]
                       [#:omit-value? omit-value? boolean? #f]
                       [#:attributes attributes attributes/c null]) widget/c]{
  Produce a widget that can render an INPUT element.
}

@defproc[(widget-checkbox [#:attributes attributes attributes/c null]) widget/c]{
  Produce a widget that can render a checkbox INPUT element.
}

@defproc[(widget-email [#:attributes attributes attributes/c null]) widget/c]{
  Produce a widget that can render a email INPUT element.
}

@defproc[(widget-file [#:attributes attributes attributes/c null]) widget/c]{
  Produce a widget that can render a file INPUT element.
}

@defproc[(widget-hidden [#:attributes attributes attributes/c null]) widget/c]{
  Produce a widget that can render a hidden INPUT element.
}

@defproc[(widget-number [#:attributes attributes attributes/c null]) widget/c]{
  Produce a widget that can render a number INPUT element.
}

@defproc[(widget-password [#:attributes attributes attributes/c null]) widget/c]{
  Produce a widget that can render a password INPUT element.
}

@defproc[(widget-select [options (or/c (hash/c string? options/c) options/c)]
                        [#:attributes attributes attributes/c null]) widget/c]{
  Produce a widget that can render a SELECT element.
}

@defproc[(widget-radio-group [options options/c]
                             [#:attributes attributes attributes/c null]) widget/c]{
  Produce a widget that can render a group of radio INPUTS.
}

@defproc[(widget-text [#:attributes attributes attributes/c null]) widget/c]{
  Produce a widget that can render a text INPUT element.
}

@defproc[(widget-textarea [#:attributes attributes attributes/c null]) widget/c]{
  Produce a widget that can render a textarea INPUT element.
}
