#lang racket/base

(require "rxexpr.rkt"
         "lib/rxexpr.rkt"
         "top.rkt"
         (for-syntax racket/base syntax/parse)
         syntax/wrap-modbeg
         racket/syntax-srcloc
         racket/match
         racket/pretty)

(provide
 doc-wrap-module-begin
 doc-main-module-begin
 default-root
 default-main
 default-extension
 default-render-from-template

 extra-dependencies
 clear-extra-dependencies!
 add-extra-dependency!
 get-extra-dependencies

 (all-from-out "rxexpr.rkt")
 ;; top.rkt
 MULTIARG-APP-TAG
 make-default-tag-proc

 mintexpp-content?
 invalid-mintexpp-syntaxes+contexts
 (struct-out errinfo)

 load-document
 some-document-has-error?
 get-document-errors
 report-document-errors
 report-and-terminate-when-error!
 )

(define (default-root locs . elements)
  (rxexpr locs 'root '() elements))

(define (default-main doc)
  (report-and-terminate-when-error!)
  (pretty-write doc))

(define default-extension #".tex")
(define (default-render-from-template doc)
  (define p (open-output-string))
  (port-count-lines! p)
  (parameterize ([current-output-port p])
    (let loop ([doc doc])
      (match doc
        [(? atomic-printable-value?)
         (pretty-display doc #:newline? #f)]
        [(? list?)
         (for-each loop doc)]
        [(rxexpr locs tag attrs elems)
         (write-char #\()
         (cond
           [(and (eq? tag MULTIARG-APP-TAG) (pair? elems))
            (loop (car elems))
            (set! elems (cdr elems))]
           [else
            (display tag)])
         (when (not (null? attrs))
           (write-string " (")
           (for ([i (in-naturals)]
                 [attr (in-list attrs)])
             (unless (zero? i)
               (write-char #\space))
             (printf "(~a ~s)" (car attr) (cadr attr)))
           (write-char #\)))
         (when (pair? elems)
           (write-char #\space)
           (loop elems))
         (write-char #\))])))
  (bytes->string/utf-8 (get-output-bytes p #t) #\uFFFD))

(define extra-dependencies
  (box '()))

(define (clear-extra-dependencies!)
  (set-box! extra-dependencies '()))

(define (add-extra-dependency! file-name)
  (set-box! extra-dependencies (cons file-name (unbox extra-dependencies))))

(define (get-extra-dependencies)
  (reverse (unbox extra-dependencies)))

(define-for-syntax the-doc '())
(define-for-syntax config-mod-name "mintexpp.rkt")

(define-syntax (stash-expr stx)
  (syntax-case stx (quote)
    [(_ (quote str))
     (string? (syntax-e #'str))
     (set! the-doc (cons (syntax-e #'str) the-doc))]
    [(_ expr)
     (set! the-doc (cons (quasisyntax/loc #'expr
                           (check-syntax! (quote-syntax #,(syntax/loc #'expr loc))
                                          (#%plain-lambda () #,(syntax-local-introduce #'expr))))
                         the-doc))])
  #'(begin))

(define-syntax (get-current-doc stx)
  #`(list #,@(for/list ([stx-or-str (in-list (reverse the-doc))])
               (if (syntax? stx-or-str)
                   (syntax-local-introduce stx-or-str)
                   stx-or-str))))

(define-syntax (doc-main-module-begin stx)
  (syntax-parse stx
    [(_ content ...)
     #:with the-root (datum->syntax stx 'root #'root)
     #:with the-extension (datum->syntax stx 'extension #'extension)
     #:with the-render-from-template (datum->syntax stx 'render-from-template #'render-from-template)
     #:with the-main (datum->syntax stx 'main #'main)
     (define-values (base name dir?)
       (if (path-string? (syntax-source stx))
           (split-path (syntax-source stx))
           (values #f #f #f)))
     (define require-config-mod
       (cond
         [(and base (file-exists? (build-path base config-mod-name)))
          #`(require #,(datum->syntax stx config-mod-name))]
         [else #'(begin)]))
     (quasisyntax/loc stx
       (#%plain-module-begin
        #,require-config-mod
        (provide doc the-extension the-render-from-template the-main)
        content ...
        (define raw-doc (get-current-doc))
        (define flat-doc (splice-content raw-doc))
        (define clean-doc (remove-nonprintable-value flat-doc))
        (define doc (apply the-root '() clean-doc))
        (module* main racket/base
          (require (submod ".."))
          (void (main doc)))))]))

(define-syntax doc-wrap-module-begin
  (make-wrapping-module-begin
   #'stash-expr
   #'doc-main-module-begin))

(define (try-unnest-multiarg-apps val)
  (match val
    [(rxexpr locs (== MULTIARG-APP-TAG) '() (cons elem elems))
     (define-values (maybe-tag maybe-attrs nested-elems)
       (try-unnest-multiarg-apps elem))
     (values maybe-tag maybe-attrs (append nested-elems elems))]
    [(rxexpr locs tag attrs elems)
     (values tag attrs elems)]
    [_ (values #f #f #f)]))

(define (mintexpp-content? val)
  (null? (invalid-mintexpp-syntaxes+contexts val)))

(define (invalid-mintexpp-syntaxes+contexts val)
  (check-recur val '()))

(define (check-list val prev ctxt)
  (cond
    [(null? val) '()]
    [else
     (define next (if (pair? (cdr val)) (cadr val) ""))
     (append (check-recur (car val) (cons `(list ,prev ,next) ctxt))
             (check-list (cdr val) (car val) ctxt))]))

(define (check-recur val ctxt)
  (match val
    [(? atomic-printable-value?)
     '()]
    [(? list? elems)
     (check-list elems "" ctxt)]
    [(rxexpr locs (== MULTIARG-APP-TAG) '() elems)
     (=> fail-next)
     (define-values (maybe-tag maybe-attrs all-elems)
       (try-unnest-multiarg-apps val))
     (if maybe-tag
         (check-list all-elems
                     "" ;; prev
                     (cons `(rxexpr/nested ,val ,maybe-tag ,maybe-attrs ,all-elems)
                           ctxt))
         (fail-next))]
    [(rxexpr locs tag attrs elems)
     (check-list elems
                 "" ;; prev
                 (cons `(rxexpr ,tag ,attrs ,elems) ctxt))]
    [_
     (list (hash 'value val 'context ctxt))]))

(struct errinfo (loc reason data) #:prefab)

(define reversed-runtime-errors '())

(define (record-runtime-error! loc reason data)
  (set! reversed-runtime-errors
        (cons (errinfo loc reason data)
              reversed-runtime-errors)))

(define (some-document-has-error?)
  (not (null? reversed-runtime-errors)))

(define (get-document-errors)
  (reverse reversed-runtime-errors))

(define (call-with-load-exception-handlers proc)
  (with-handlers ([exn:fail:read?
                   (λ (e)
                     (record-runtime-error! #f 'read e)
                     #f)]
                  [exn:fail:filesystem:missing-module?
                   (λ (e)
                     (record-runtime-error! #f 'missing-module e)
                     #f)]
                  [exn:fail?
                   (λ (e)
                     (record-runtime-error! #f 'exn e)
                     #f)])
    (proc)))

(define (load-document file-name)
  (call-with-load-exception-handlers
   (λ () (dynamic-require file-name 'doc))))

(define (display-error/replace-message new-message old-exn)
  ((error-display-handler) new-message (exn:fail new-message (exn-continuation-marks old-exn))))

(define (report-document-errors doc-errs)
  (for ([err (in-list doc-errs)])
      (match (errinfo-reason err)
        ['read
         (define e (errinfo-data err))
         (display-error/replace-message
          (string-append "fail to parse the document: " (exn-message e))
          e)]
        ['missing-module
         (define e (errinfo-data err))
         (display-error/replace-message
          (string-append "document not found: " (exn-message e))
          e)]
        ['exn
         (define e (errinfo-data err))
         (display-error/replace-message
          (cond [(errinfo-loc err)
                 (string-append "error near "
                                (srcloc->string (syntax-srcloc (errinfo-loc err)))
                                ": "
                                (exn-message e))]
                [else (string-append "error: " (exn-message e))])
          e)]
        ['syntax
         (eprintf "error near ~a: " (srcloc->string (syntax-srcloc (errinfo-loc err))))
         (eprintf "non-printable value: ~s\n" (hash-ref (errinfo-data err) 'value))
         (when (not (null? (hash-ref (errinfo-data err) 'context)))
           (eprintf "  in...:\n")
           (for ([frame (in-list (hash-ref (errinfo-data err) 'context))])
             (match frame
               [`(list ,prev ,next)
                (format "   ~a●~a\n" prev next)]
               [`(rxexpr/nested ,orig ,tag ,attrs ,all-elems)
                (if (null? attrs)
                    (eprintf "   (~a ●)\n" tag)
                    (eprintf "   (~a ~s ●)\n" tag attrs))]
               [`(rxexpr ,tag ,attrs ,elems)
                (if (null? attrs)
                    (eprintf "   (~a ●)\n" tag)
                    (eprintf "   (~a ~s ●)\n" tag attrs))])))])))

(define (report-and-terminate-when-error!)
  (when (some-document-has-error?)
    (report-document-errors (get-document-errors))
    (exit 1)))

(define (check-syntax! loc make-val)
  (with-handlers ([exn:fail? (λ (e)
                               (record-runtime-error! loc 'exn e)
                               "")])
    (define val (make-val))
    (define invalid-syntaxes+contexts
      (invalid-mintexpp-syntaxes+contexts val))
    (unless (null? invalid-syntaxes+contexts)
      (for ([invalid-syntax+context (in-list invalid-syntaxes+contexts)])
        (record-runtime-error! loc 'syntax invalid-syntax+context)))
    val))
