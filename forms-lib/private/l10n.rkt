#lang racket/base

(require racket/contract
         srfi/29)

(provide translate)

(define bundle 'forms-lib)

(define translations
  '([(en) . ([err-required      . "This field is required."]
             [err-matches       . "This field must match the regular expression ~v."]
             [err-one-of        . "This field must contain one of the following values: ~a"]
             [err-shorter-than  . "This field must contain ~a or fewer characters."]
             [err-longer-than   . "This field must contain ~a or more characters."]
             [err-to-number     . "This field must contain a number."]
             [err-binding/email . "This field must contain an e-mail address."])]
    [(ro) . ([err-required      . "Acest câmp este obligatoriu."]
             [err-matches       . "Acest câmp trebuie să aibă forma ~v."]
             [err-one-of        . "Acest câmp trebuie să conțină una din valorile următoare: ~a"]
             [err-shorter-than  . "Acest câmp trebuie să conțină ~a sau mai puține caractere."]
             [err-longer-than   . "Acest câmp trebuie să conțină ~a sau mai multe caractere."]
             [err-to-number     . "Acest câmp trebuie să conțină un număr."]
             [err-binding/email . "Acest câmp trebuie să conțină o adresă de e-mail."])]))

(for ([translation (in-list translations)])
  (define locale (car translation))
  (define translations (cdr translation))
  (define specifier (apply list bundle locale))
  (unless (load-bundle! specifier)
    (declare-bundle! specifier translations)
    (store-bundle! specifier)))

(define/contract (translate message-name . args)
  (-> symbol? any/c ... string?)
  (cond
    [(localized-template bundle message-name)
     => (lambda (message)
          (apply format message args))]

    [else (symbol->string message-name)]))
