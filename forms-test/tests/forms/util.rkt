#lang racket/base

(require net/url
         racket/promise
         web-server/http)

(provide (all-defined-out))

(define (make-binding s #:name [name #"fake-binding"])
  (binding:form name (string->bytes/utf-8 s)))

(define (make-request #:method [method #"GET"]
                      #:uri [uri (string->url "http://example.com")]
                      #:headers [headers (list)]
                      #:bindings [bindings (list)])
  (request method uri headers (delay bindings) #f "127.0.0.1" 8000 "127.0.0.1"))
