#lang racket/base

(require (for-syntax racket/base racket/syntax syntax/parse)
         mintexpp/rxexpr mintexpp/top)

(provide define-constant-commands
         define-function-commands
         define-function-command)

(define-syntax (define-constant-commands stx)
  (syntax-parse stx
    [(_ command:id ...)
     #:with (defs ...) (for/list ([cmd (in-list (syntax-e #'(command ...)))])
                         (quasisyntax/loc cmd
                           (define #,cmd (free-literal '#,cmd))))
     #'(begin defs ...)]))

(define-syntax (define-function-commands stx)
  (syntax-parse stx
    [(_ (command:id arg:id ...+) ...)
     #:with (defs ...) (for/list ([cmd (in-list (syntax-e #'(command ...)))]
                                  [args (in-list (syntax-e #'((arg ...) ...)))])
                         (quasisyntax/loc cmd
                           (define-function-command (#,cmd . #,args))))
     #'(begin defs ...)]))

(define-syntax (define-function-command stx)
  (syntax-parse stx
    [(_ (command:id arg:id ...+))
     (define arg-stxs (syntax-e #'(arg ...)))
     (define loc-temps
       (generate-temporaries
        (for/list ([arg-stx (in-list arg-stxs)])
          (format-id #'here "locs-~a" arg-stx))))

     (define hdr-stx
       (for/fold ([hdr-stx (quasisyntax/loc #'command
                             (command #,(car loc-temps) . #,(car arg-stxs)))])
                 ([arg-stx (in-list (cdr arg-stxs))]
                  [loc-temp (in-list (cdr loc-temps))])
         (quasisyntax/loc arg-stx
           (#,hdr-stx #,loc-temp . #,arg-stx))))

     (define appexpr-stx
       (for/fold ([appexpr-stx (quasisyntax/loc #'command
                                 (rxexpr #,(car loc-temps) 'command null #,(car arg-stxs)))])
                 ([arg-stx (in-list (cdr arg-stxs))]
                  [loc-temp (in-list (cdr loc-temps))])
         (quasisyntax/loc arg-stx
           (rxexpr #,loc-temp
                   MULTIARG-APP-TAG
                   null
                   (cons #,appexpr-stx #,arg-stx)))))

     (quasisyntax/loc stx
       (define/loc #,hdr-stx #,appexpr-stx))]))
