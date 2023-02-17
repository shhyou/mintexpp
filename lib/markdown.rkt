#lang racket/base

(require racket/match

         racket/list
         racket/hash

         racket/pretty
         racket/date

         commonmark/parse commonmark/struct

         "../rxexpr.rkt"
         "../tex.rkt"
         "embed.rkt")

(provide
 *PRINT-ENRICHING-DEBUG-LOG*

 enrich-control-attr
 default-inline-handler
 default-block-handler
 default-flow-handler
 (struct-out converted-paragraph)

 current-inline-handler
 current-block-handler
 current-flow-handler

 enrich-with-markdown)

(define-logger markdown-enriching)

(define *PRINT-ENRICHING-DEBUG-LOG* (make-parameter #f))
(define enrich-control-attr (make-parameter 'verbatim))

;; inline-handler : inline? -> (listof stringified-rxexpr?)
(define (default-inline-handler inline-handler x)
  (match x
    [(? string? s)
     (list s)]
    [(? list? xs)
     (append* (map inline-handler xs))]
    [(italic content)
     (list (apply emph '() (inline-handler content)))]
    [(bold content)
     (list (apply textbf '() (inline-handler content)))]
    [(code str)
     (list (apply texttt '() (list str)))]
    [(link content dest title)
     (list
      ((rxexpr/app '() 'href '() (list dest))
       '()
       (inline-handler content)))]
    [(? line-break?)
     (list |\\|)]
    ;; (image desc src title), (footnote-reference lbl) and html
    [content
     (error 'default-inline-handler "unhandled markdown element: ~e" content)]))

(struct converted-paragraph (content) #:transparent)

;; flow-handler : (listof block?) -> (listof stringified-rxexpr?)
;; inline-handler : inline? -> (listof stringified-rxexpr?)
(define (default-block-handler flow-handler inline-handler b)
  (match b
    [(paragraph content)
     (converted-paragraph (inline-handler content))]
    [(itemization blockss style (and start-num (or 1 #f)))
     (apply (if start-num enumerate itemize)
            '()
            (for/list ([content (in-list blockss)])
              (apply item '() (flow-handler content))))]
    [(blockquote content)
     (apply lquote '() (flow-handler content))]
    [(code-block str lang)
     (apply verbatim '() (list str))]
    [(heading title (and depth (or 1 2 3)))
     (apply (vector-ref `#[#f ,section ,subsection ,subsubsection] depth)
            '()
            (inline-handler title))]
    [(? thematic-break?)
     hrule]
    ;; (html-block content)
    [content
     (error 'default-flow-handler "unhandled markdown block: ~e" content)]))

;; flow-handler : (listof block?) -> (listof stringified-rxexpr?)
;; inline-handler : inline? -> (listof stringified-rxexpr?)
(define (default-flow-handler flow-handler inline-handler blocks)
  (define block-handler (current-block-handler))
  (define converted-blocks
    (for/list ([b (in-list blocks)])
      (block-handler flow-handler inline-handler b)))
  (append*
   (for/list ([previous-block (in-list (cons #f blocks))]
              [current-block (in-list blocks)]
              [converted-block (in-list converted-blocks)])
     (match converted-block
       [(converted-paragraph converted-content)
        (cond
          [(or (paragraph? previous-block)
               (thematic-break? previous-block))
           (cons '"\n\n" converted-content)]
          [previous-block
           (cons '"\n" converted-content)]
          [else
           converted-content])]
       [converted-block
        (cond
          [(and previous-block (heading? current-block) (= 1 (heading-depth current-block)))
           (list "\n\n" converted-block)]
          [previous-block
           (list "\n" converted-block)]
          [else
           (list converted-block)])]))))

(define current-inline-handler (make-parameter default-inline-handler))
(define current-flow-handler (make-parameter default-flow-handler))
(define current-block-handler (make-parameter default-block-handler))

(define (handle-inline x)
  ((current-inline-handler) handle-inline x))

(define (handle-flow blocks)
  ((current-flow-handler) handle-flow handle-inline blocks))

(define (enrich-with-markdown rx)
  (define ctlattr (enrich-control-attr))

  (log-markdown-enriching-info "[~a] start embedding" (date->string (current-date) #t))
  (define-values (rx-id embedding)
    (parameterize ([current-embedding (make-hash)])
      (define rx-id (embed-stringified-rxexpr rx))
      (values rx-id (current-embedding))))
  (log-markdown-enriching-info "[~a] finished embedding" (date->string (current-date) #t))

  (define verbatim-embedding
    (for/hash ([(embed-id x) (in-hash embedding)]
               #:when (or (not (rxexpr? x))
                          (and ctlattr (attr-ref (rxexpr-attrs x) ctlattr (λ () #f)))))
      (values embed-id x)))

  (log-markdown-enriching-info "[~a] start enriching markdown" (date->string (current-date) #t))
  (define enriched-embedding
    (for/hash ([(embed-id x) (in-hash embedding)]
               #:when (or (not ctlattr)
                          (and (rxexpr? x)
                               (not (attr-ref (rxexpr-attrs x) ctlattr (λ () #f))))))
      (define all-str (apply string-append (rxexpr-elements x)))
      (define markdown-doc (string->document all-str))
      (when (*PRINT-ENRICHING-DEBUG-LOG*)
        (printf "~s: ~s\n    " embed-id all-str)
        (pretty-write (document-blocks markdown-doc)))
      (unless (null? (document-footnotes markdown-doc))
        (log-markdown-enriching-warning
         (date->string (current-date) #t)
         "[~a] warning: document contains footnote(s): ~e"
         (document-footnotes markdown-doc)))
      (define enriched-elements
        (handle-flow (document-blocks markdown-doc)))
      (values embed-id
              (rxexpr (rxexpr-locs x)
                      (rxexpr-tag x)
                      (rxexpr-attrs x)
                      enriched-elements))))
  (log-markdown-enriching-info "[~a] finished enriching" (date->string (current-date) #t))

  (define full-embedding
    (hash-union verbatim-embedding enriched-embedding))

  (log-markdown-enriching-info
   "[~a] start un-embedding strings: total ~a"
   (date->string (current-date) #t)
   (hash-count full-embedding))
  (match-define (list enriched-unembedded-rx)
    (parameterize ([current-embedding full-embedding])
      (unembed-rxexprs-in-string rx-id)))
  (log-markdown-enriching-info "[~a] finished un-embedding strings" (date->string (current-date) #t))

  enriched-unembedded-rx)
