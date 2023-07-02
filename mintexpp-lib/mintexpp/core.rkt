#lang racket/base

(require "main.rkt"
         "top.rkt"
         "rxexpr.rkt")

(provide
 (except-out (all-from-out racket/base)
             date
             #%module-begin
             #%top)
 (all-from-out "rxexpr.rkt")
 make-default-tag-proc
 MULTIARG-APP-TAG
 (rename-out [doc-wrap-module-begin #%module-begin]
             [literal-top #%top]
             [default-root root]
             [default-main main]
             [default-extension extension]
             [default-render-from-template render-from-template])
 add-extra-dependency!)

(module reader syntax/module-reader

  ;; module path
  mintexpp/core

  #:info mintexpp-core-info

  #:whole-body-readers? #t

  #:read (λ (inp)
           (port-count-lines! inp)
           ((dynamic-require 'scribble/reader 'read-inside) inp))

  #:read-syntax (λ (src inp)
                  (port-count-lines! inp)
                  ((dynamic-require 'scribble/reader 'read-syntax-inside) src inp))

  (define (mintexpp-core-info key defval default)
    (case key
      [(color-lexer)
       ((dynamic-require 'syntax-color/scribble-lexer 'make-scribble-inside-lexer))]
      [(drracket:indentation)
       (dynamic-require 'scribble/private/indentation 'determine-spaces)]
      [(drracket:default-filters)
       `(["Preprocessed Article" "*.mtd"])]
      [(drracket:default-extension)
       "mtd"]
      [else
       (default key defval)]))
  )
