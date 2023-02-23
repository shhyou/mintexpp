;; Modified from syntax/parse/lib/function-header
;; Added the application syntax to the formals syntax classes
#lang racket/base

(require racket/list racket/syntax syntax/parse syntax/parse/lib/function-header)

(provide formals+app-no-rest formals+app)

(define-splicing-syntax-class formals+app-no-rest
  #:attributes (params apps)
  (pattern (~seq arg:formal ...)
    #:attr params #'(arg.name ...)
    #:attr apps #'((~? (~@ arg.kw arg.name) arg.name) ...)
    #:fail-when (check-duplicate-identifier (syntax->list #'params))
    "duplicate argument name"
    #:fail-when (check-duplicates (attribute arg.kw)
                                  (lambda (x y)
                                    (and x y (equal? (syntax-e x) (syntax-e y)))))
    "duplicate keyword for argument"
    ;; invalid-option-placement is not exported
    #|
    #:fail-when (invalid-option-placement
                 (attribute arg.kw) (attribute arg.name) (attribute arg.default))
    "default-value expression missing"
    |#
    ))

(define-syntax-class formals+app
  #:attributes (params apps)
  (pattern (~or* (args:formals+app-no-rest)
                 (args:formals+app-no-rest . rest-id:id))
    #:attr params #'((~@ . args.params) (~? rest-id))
    #:attr apps #'((~@ . args.apps) (~? rest-id))
    #:fail-when (and (attribute rest-id)
                     (member #'rest-id (syntax->list #'args.params) bound-identifier=?)
                     #'rest-id)
    "duplicate argument identifier"))
