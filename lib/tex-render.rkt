#lang racket/base

(require racket/list
         racket/string
         racket/set
         racket/match
         "../rxexpr.rkt"
         "../top.rkt"
         "../srclocplus.rkt"
         "../at-reader.rkt")

(provide pretty-print-newline-tex-commands
         is-pretty-print-newline-tex-command?
         environment-tex-commands
         is-environment-tex-command?

         default-rxexpr-render-handler
         rxexpr-render-handler
         rxexprs->tree
         rxexprs->string
         )

(define pretty-print-newline-tex-commands
  (make-parameter (seteq)))

(define (is-pretty-print-newline-tex-command? tag)
  (set-member? (pretty-print-newline-tex-commands) tag))

(define environment-tex-commands
  (make-parameter (seteq)))

(define (is-environment-tex-command? tag)
  (set-member? (environment-tex-commands) tag))

;; Turns attrs into tree, recognizing the 'datums attr
;; note: must return a list
(define (rxexpr-attrs->tree handle attrs)
  (cond
    [(not (hash-empty? attrs))
     (define kv-options
       (append*
        (for/list ([(key value) (in-hash (hash-remove attrs 'datums))])
          (list "," (format "~a=" key) (handle value)))))
     (cond
       [(hash-has-key? attrs 'datums)
        (append '("[") (list (handle (hash-ref attrs 'datums))) kv-options '("]"))]
       [else
        (append '("[") (cdr kv-options) '("]"))])]
    [else '()]))

(define (default-rxexpr-render-handler handle x)
  (match x
    [(? list? xs) (map handle x)]
    [(? string? str) str]
    [(? number? n) (number->string n)]
    [(? symbol? a) (symbol->string a)]
    [(free-literal '|\\|) "\\\\"]
    [(free-literal id)    (format "\\~a" id)]
    [(rxexpr locs (== SPLICE-TAG) attrs elements)
     (map handle elements)]
    [(rxexpr locs '$ attrs elements)
     (cond
       [(null? elements)
        ;; TODO: report error
        (eprintf "default-rxexpr-render-handler: $: empty math command\n")
        (define error-loc (ormap cdr locs))
        (when error-loc
          (eprintf " at: ~a:~s:~s\n"
                   (srclocplus-source error-loc)
                   (srclocplus-line error-loc)
                   (srclocplus-column error-loc)))
        ""]
       [else
        `("$" ,(handle elements) "$")])]
    [(rxexpr locs '$$ attrs elements)
     `("\\["
       ,(handle elements)
       "\\]")]
    [(? rxexpr? rx-orig)
     ;; TODO: check if there are LaTeX commands like
     ;;   \mycommand{arg}[trailing-opt]
     (define-values (tag attrss elementss)
       (let loop ([rx rx-orig]
                  [outer-attrss '()]
                  [outer-elementss '()])
         (cond
           [(eq? (rxexpr-tag rx) MULTIARG-APP-TAG)
            (when (null? (rxexpr-elements rx))
              (error 'default-rxexpr-render-handler
                     "invalid nested rxexpr: empty nested rxexpr\n value: ~e"
                     rx-orig))
            (loop (car (rxexpr-elements rx))
                  (cons (attrs->hash (rxexpr-attrs rx)) outer-attrss)
                  (cons (cdr (rxexpr-elements rx)) outer-elementss))]
           [(rxexpr? rx)
            (values (rxexpr-tag rx)
                    (cons (attrs->hash (rxexpr-attrs rx)) outer-attrss)
                    (cons (rxexpr-elements rx) outer-elementss))]
           [else
            (error 'default-rxexpr-render-handler
                   "invalid nested rxexpr: the innermost expression is not an rxexpr\n value: ~e"
                   rx-orig)])))
     (define datumss
       (for/list ([attrs (in-list attrss)])
         (rxexpr-attrs->tree handle attrs)))
     (define handled-elementss
       (for/list ([elements (in-list elementss)])
         (handle elements)))
     (cond
       [(is-environment-tex-command? tag)
        (define arity
          (length handled-elementss))
        (append (list (format "\\begin{~a}" tag))
                (append*
                 (for/list ([i (in-naturals 1)]
                            [datums (in-list datumss)]
                            [handled-elements (in-list handled-elementss)])
                   (cond
                     [(= i arity)
                      (append datums
                              (if (and (pair? handled-elements)
                                       (string? (car handled-elements))
                                       (string-prefix? (car handled-elements) "\n"))
                                  '("")
                                  '("\n"))
                              handled-elements)]
                     [else
                      (append datums
                              '("{")
                              handled-elements
                              '("}"))])))
                (list (format "\\end{~a}\n" tag)))]
       [else
        (append* (list (format "\\~a" tag))
                 (for/list ([datums (in-list datumss)]
                            [handled-elements (in-list handled-elementss)])
                   (append datums
                           '("{") handled-elements '("}")
                           (if (is-pretty-print-newline-tex-command? tag)
                               '("\n")
                               '()))))])]))

(define rxexpr-render-handler
  (make-parameter default-rxexpr-render-handler))

(define (rxexprs->tree x)
  (define rx-handler (rxexpr-render-handler))
  (define (do-rxexpr->tree x) (rx-handler do-rxexpr->tree x))
  (do-rxexpr->tree x))

(define (rxexprs->string x)
  (apply string-append (flatten (rxexprs->tree x))))
