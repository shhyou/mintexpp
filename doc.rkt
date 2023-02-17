#lang racket/base

(require "main.rkt"
         "top.rkt"
         "rxexpr.rkt"
         "tex.rkt"
         (for-syntax racket/base))

(provide
 (except-out (all-from-out racket/base)
             date
             #%module-begin
             #%app
             #%top)
 (all-from-out "rxexpr.rkt")
 (all-from-out "tex.rkt")
 make-default-tag-proc
 MULTIARG-APP-TAG
 (rename-out [doc-wrap-module-begin #%module-begin]
             [literal-top #%top]
             [rx/app-locs-and-quote-splicing-string-app #%app]
             [default-root root]
             [default-main main]
             [default-extension extension]
             [default-render-from-template render-from-template])
 add-extra-dependency!)

(define-syntax (rx/app-locs-and-quote-splicing-string-app stx)
  (syntax-case stx ()
    [(_ content other-datum ...)
     (string? (syntax-e #'content))
     (syntax/loc stx (@ content other-datum ...))]
    [(_ app-tgt app-expr ...)
     (let ()
       (define srcpluss (syntax-property stx 'at-exp-srclocplus))
       (define is-rxexpr-app?
         (or
          ;; free literals
          (and (identifier? #'app-tgt) (not (identifier-binding #'app-tgt)))
          ;; repeated at-expressions; read as nested applications
          (and (syntax-property #'app-tgt 'at-exp-srclocplus)
               (pair? (syntax-e #'app-tgt)))))
       (cond
         [is-rxexpr-app?
          (quasisyntax/loc stx
            (#%app app-tgt
                   #,(if srcpluss (stage-at-exp-srclocplus srcpluss) #'null)
                   app-expr ...))]
         [else
          (syntax/loc stx (#%app app-tgt app-expr ...))]))]
    [(_ app-expr ...)
     (syntax/loc stx (#%app app-expr ...))]))

