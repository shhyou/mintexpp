#lang racket/base

(require mintexpp/core
         "defs.rkt"
         (for-syntax racket/base mintexpp/at-reader))

(provide
 (except-out (all-from-out mintexpp/core)
             #%app)
 (all-from-out "defs.rkt")
 (rename-out [rx/app-locs-and-quote-splicing-string-app #%app]))

(define-syntax (rx/app-locs-and-quote-splicing-string-app stx)
  (syntax-case stx ()
    [(_ content other-datum ...)
     (or (string? (syntax-e #'content))
         (let ([srcpluss (syntax-property stx 'at-exp-srclocplus)])
           (and (vector? srcpluss)
                (not (at-exp-cmd-loc (vector->list srcpluss))))))
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

