#lang racket/base

(require racket/match
         (for-syntax racket/base racket/list racket/match racket/syntax syntax/transformer
                     syntax/parse "function-header.rkt"
                     mintexpp/srclocplus)
         syntax/parse/define
         mintexpp/srclocplus)

(provide (struct-out rxexpr)
         rxexpr-guard
         attr-ref
         attr-set
         attr-remove
         attrs->hash
         rxexpr-attr-remove

         MULTIARG-APP-TAG
         make-default-tag-proc
         rxexpr/app

         (for-syntax rxwrapper rxwrapper? rxwrapper-internal-fun rxwrapper-internal-id
                     stage-at-exp-srclocplus)
         define/loc

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

(define (attr-remove attrs key)
  (for/list ([attr (in-list attrs)]
             #:when (not (eq? (car attr) key)))
    attr))

(define (attr-set attrs key new-value)
  (cons (list key new-value)
        (attr-remove attrs key)))

(define (attrs->hash attrs)
  (for/hash ([attr (in-list attrs)])
    (values (car attr) (cadr attr))))

(define (rxexpr-attr-remove x key)
  (if (rxexpr? x)
      (rxexpr (rxexpr-locs x)
              (rxexpr-tag x)
              (attr-remove (rxexpr-attrs x) key)
              (rxexpr-elements x))
      x))

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

(define (make-locs-assoc-list . args)
  (let loop ([args args])
    (match args
      ['() '()]
      [(list* (? symbol? name) #f args)
       (cons `(,name . #f) (loop args))]
      [(list* (? symbol? name) (? syntax? loc-stx) diff args)
       (cons `(,name . ,(if (exact-integer? diff)
                            (syntax+diff->srclocplus loc-stx (srclocdiff 0 diff))
                            (syntax+diff->srclocplus loc-stx diff)))
             (loop args))])))

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
    (define locs-args
      (append*
       (for/list ([assoc-loc (in-vector srcpluss)]
                  #:when (and (cdr assoc-loc)
                              (not (zero? (srclocplus-span (cdr assoc-loc))))))
         (define-values (stx locdiff)
           (srclocplus->syntax+diff (cdr assoc-loc)))
         (list #`'#,(car assoc-loc)
               #`(quote-syntax #,stx)
               #`'#,(if (zero? (srclocdiff-line-span locdiff))
                        (srclocdiff-next-column locdiff)
                        locdiff)))))
    (if (positive? (vector-length srcpluss))
        #`(make-locs-assoc-list . #,locs-args)
        #'null))

  (struct rxwrapper (internal-fun internal-id)
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
                          (cond
                            [srcpluss
                             (list* (rxwrapper-internal-fun self)
                                    (stage-at-exp-srclocplus srcpluss)
                                    (syntax-e #'(args ...)))]
                            [(cons (rxwrapper-internal-id self) (syntax-e #'(args ...)))])
                          stx))]))
    #:transparent)

  (define-syntax-class (nary-locs-header ctxt-stx)
    #:attributes (name internal-fun-id papp-fun-id internal-fun-header papp-header papp-expr)
    [pattern (name:id loc:id . fmls:formals+app)
      #:with internal-fun-id (format-id ctxt-stx "~a.mk" #'name #:source #'name)
      #:with papp-fun-id (format-id ctxt-stx "~a.mk/locs" #'name #:source #'name)
      #:with args #'(loc . fmls)
      #:with internal-fun-header #'(internal-fun-id loc . fmls)
      #:with papp-header #'(papp-fun-id . fmls)
      #:with papp-expr (if (list? (syntax-e #'fmls))
                           #`(internal-fun-id null . fmls.apps)
                           #`(apply internal-fun-id null . fmls.apps))]
    [pattern ((~var header* (nary-locs-header ctxt-stx)) loc:id . fmls:formals+app)
      #:with name #'header*.name
      #:with internal-fun-id #'header*.internal-fun-id
      #:with papp-fun-id #'header*.papp-fun-id
      #:with args #'(loc . fmls)
      #:with internal-fun-header #'(header*.internal-fun-header loc . fmls)
      #:with papp-header #'(header*.papp-header . fmls)
      #:with papp-expr (if (list? (syntax-e #'fmls))
                           #`(header*.papp-expr null . fmls.apps)
                           #`(apply header*.papp-expr null . fmls.apps))])
  )

(define-syntax-parse-rule (define/loc (~var header (nary-locs-header #'here))
                            body-expr:expr
                            ...+)
  #:with fun-impl-def (syntax/loc this-syntax
                        (define header.internal-fun-header
                          body-expr
                          ...))
  (begin
    fun-impl-def
    (define header.papp-header header.papp-expr)
    (define-syntax header.name
      (rxwrapper (quote-syntax header.internal-fun-id) (quote-syntax header.papp-fun-id)))))

(define SPLICE-TAG '@)
(define/loc (@ locs . elements) (rxexpr locs SPLICE-TAG '() elements))

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