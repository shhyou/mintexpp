#lang racket/base

(require (only-in "private/rxexpr.rkt" make-default-tag-proc rxexpr/app)
         (for-syntax racket/base syntax/stx)
         racket/struct)

(provide
 (struct-out free-literal)
 literal-top)

(struct free-literal (id)
  #:property prop:procedure (make-default-tag-proc
                             (λ (self locs attrs elements)
                               (rxexpr/app locs (free-literal-id self) attrs elements)))
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (define free-literal-repn (free-literal-id self))
     (case mode
       [(#t) (write free-literal-repn port)]
       [(#f) (display free-literal-repn port)]
       [(0 1) (print free-literal-repn port mode)]))]
  #:transparent)

(define-syntax (literal-top stx)
  (define id (stx-cdr stx))
  (syntax-property
   (datum->syntax #'here `(,#'free-literal (,#'quote ,id)) stx)
   'mouse-over-tooltips (vector (syntax-local-introduce id)
                                (sub1 (syntax-position id))
                                (sub1 (+ (syntax-position id) (syntax-span id)))
                                (format "~a" (syntax-e id)))))
