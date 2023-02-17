#lang racket/base

(require racket/match
         (for-syntax racket/base racket/match racket/syntax syntax/transformer
                     syntax/parse syntax/parse/lib/function-header
                     "srclocplus.rkt")
         syntax/parse/define
         "srclocplus.rkt")

(provide (struct-out rxexpr)
         rxexpr-guard
         attr-ref
         attrs->hash

         MULTIARG-APP-TAG
         make-default-tag-proc
         rxexpr/app

         (for-syntax rxwrapper rxwrapper? rxwrapper-internal-id
                     stage-at-exp-srclocplus)
         define-unary/loc
         define-binary/loc
         define-ternary/loc
         (rename-out [define-ternary/loc define-trinary/loc])

         SPLICE-TAG
         @
         splice-content)

(define rxexpr-ctcs
  (list (match-lambda
          [`((,(? symbol?) . ,(or #f (? srclocplus?))) ...) #t]
          [_ #f])
        symbol?
        (match-lambda
          [`((,(? symbol?) ,_) ...) #t]
          [_ #f])
        list?))

(define rxexpr-ctcs-name
  (list "(listof (cons/c symbol? (or/c #f srclocplus?)))"
        "symbol?"
        "(listof (list/c symbol? any/c))"
        "list?"))

(define (rxexpr-guard locs tag attrs elements name)
  (define bad-pos
    (for/or ([i (in-naturals)]
             [val (in-list (list locs tag attrs elements))]
             [pred? (in-list rxexpr-ctcs)])
      (and (not (pred? val)) i)))
  (when bad-pos
    (raise-argument-error name
                          (list-ref rxexpr-ctcs-name bad-pos)
                          bad-pos
                          locs
                          tag
                          attrs
                          elements))
  (values locs tag attrs elements))

(define (attr-ref attrs key [fail (λ () (error 'attr-ref "no value found for key\n  key: ~a" key))])
  (define found (assoc key attrs))
  (cond
    [found (cadr found)]
    [(and (procedure? fail) (procedure-arity-includes? fail 0))
     (fail)]
    [else fail]))

(define (attrs->hash attrs)
  (for/hash ([attr (in-list attrs)])
    (values (car attr) (cadr attr))))

(struct rxexpr (locs tag attrs elements)
  #:guard rxexpr-guard
  ; #:property prop:custom-print-quotable 'maybe
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (define rxexpr-repn
       (cons (rxexpr-tag self)
             (if (null? (rxexpr-attrs self))
                 (rxexpr-elements self)
                 (cons (rxexpr-attrs self) (rxexpr-elements self)))))
     (case mode
       [(#t) (write rxexpr-repn port)]
       [(#f) (display rxexpr-repn port)]
       [(0 1) (print rxexpr-repn port mode)]))]
  #:transparent)

(define MULTIARG-APP-TAG '#%app)

(define (make-default-tag-proc ctor)
  (make-keyword-procedure
   (λ (kws kw-vals self locs . elements)
     (ctor self
           locs
           (for/list ([kw (in-list kws)]
                      [val (in-list kw-vals)])
             (list (string->symbol (keyword->string kw)) val))
           elements))))

(struct rxexpr/app rxexpr ()
  #:property prop:procedure (make-default-tag-proc
                             (λ (self locs attrs elements)
                               (rxexpr/app locs MULTIARG-APP-TAG attrs (cons self elements))))
  #:transparent)

(begin-for-syntax
  ;; XXX TODO srcpluss's in the syntax properties may be combined from macro invocations
  (define (stage-at-exp-srclocplus srcplusss)
    (define srcpluss
      (let find-car-loop ([srcplusss srcplusss])
        (match srcplusss
          [(? vector? srcpluss) srcpluss]
          [(cons ar dr) (find-car-loop ar)])))
    (define all-equal?
      (let eq-loop? ([srcplusss srcplusss])
        (match srcplusss
          [(? vector? srcpluss2) (equal? srcpluss srcpluss2)]
          [(cons ar dr) (and (eq-loop? ar) (eq-loop? dr))])))
    (unless all-equal?
      (log-warning "rxexpr:stage-at-exp-srclocplus: source locations are not equal: ~s" srcplusss))
    (define locs
      (for/list ([assoc-loc (in-vector srcpluss)])
        (cond
          [(cdr assoc-loc)
           (define-values (stx locdiff)
             (srclocplus->syntax+diff (cdr assoc-loc)))
           #`(cons '#,(car assoc-loc)
                   (syntax+diff->srclocplus (quote-syntax #,stx) '#,locdiff))]
          [else
           #`'(#,(car assoc-loc) . #f)])))
    #`(list . #,locs))

  (struct rxwrapper (internal-id)
    #:property prop:procedure
    (λ (self stx)
      (syntax-parse stx
        [tgt:id
         (with-disappeared-uses (record-disappeared-uses #'tgt)
           (rxwrapper-internal-id self))]
        [(tgt:id args ...)
         (define srcpluss (syntax-property stx 'at-exp-srclocplus))
         (with-disappeared-uses (record-disappeared-uses #'tgt)
           (datum->syntax stx
                          (list* (rxwrapper-internal-id self)
                                 (if srcpluss (stage-at-exp-srclocplus srcpluss) #'null)
                                 (syntax-e #'(args ...)))
                          stx))]))
    #:transparent))

(define-syntax-parse-rule (define-unary/loc (~and (name:id . _)
                                                  header:function-header)
                            body-expr:expr
                            ...+)
  #:with local-name (format-id #'here "~a.mk-rxexpr" #'header.name #:source #'header.name)
  (begin
    (define (local-name . header.args)
      body-expr
      ...)
    (define-syntax header.name
      (rxwrapper (quote-syntax local-name)))))

(define-syntax-parse-rule (define-binary/loc (~and ((name:id . _) . _)
                                                   header:function-header
                                                   (header*:function-header . _))
                            body-expr:expr
                            ...+)
  #:with local-name (format-id #'here "~a.mk-rxexpr" #'header.name #:source #'header.name)
  (begin
    (define ((local-name . header*.args) . header.args)
      body-expr
      ...)
    (define-syntax header.name
      (rxwrapper (quote-syntax local-name)))))

(define-syntax-parse-rule (define-ternary/loc (~and (((name:id . _) . _) . _)
                                                    header:function-header
                                                    (header*:function-header . _)
                                                    ((header**:function-header . _) . _))
                            body-expr:expr
                            ...+)
  #:with local-name (format-id #'here "~a.mk-rxexpr" #'header.name #:source #'header.name)
  (begin
    (define (((local-name . header**.args) . header*.args) . header.args)
      body-expr
      ...)
    (define-syntax header.name
      (rxwrapper (quote-syntax local-name)))))

(define SPLICE-TAG '@)
(define-unary/loc (@ locs . elements) (rxexpr locs SPLICE-TAG '() elements))

;; Splice the '@' tags while preserving list-ness
;; That is, if the input is a list then the output will be a list
(define (splice-content elem)
  (match elem
    [(? list?)
     (let splice-loop ([elem elem])
       (cond
         [(null? elem) elem]
         [else
          (define spliced-car-elem (splice-content (car elem)))
          (define spliced-cdr-elems (splice-content (cdr elem)))
          (if (list? spliced-car-elem)
              (append spliced-car-elem spliced-cdr-elems)
              (cons spliced-car-elem spliced-cdr-elems))]))]
    [(rxexpr locs (== SPLICE-TAG) '() (list elem))
     (splice-content elem)]
    [(rxexpr locs (== SPLICE-TAG) '() sub-elems)
     (splice-content sub-elems)]
    [(rxexpr locs tag `((,keys ,vals) ...) sub-elems)
     (rxexpr locs
             tag
             (for/list ([key (in-list keys)]
                        [val (in-list vals)])
               (list key (splice-content val)))
             (splice-content sub-elems))]
    [_ elem]))
