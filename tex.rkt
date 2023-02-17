#lang racket/base

(require "rxexpr.rkt" "top.rkt" "srclocplus.rkt")

(provide
 (except-out (all-defined-out)
             the-documentclass-options
             the-documentclass-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commands that the markdown library needs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define hrule (free-literal 'hrule))
(define hfill (free-literal 'hfill))

(define-unary/loc (chapter locs . elements)
  (rxexpr locs 'chapter '() elements))

(define-unary/loc (chapter* locs . elements)
  (@ (free-literal 'phantomsection)
     (rxexpr locs 'chapter* '() elements)
     (rxexpr '() 'addcontentsline '((param "toc}{chapter")) elements)))

(define-unary/loc (section locs . elements)
  (rxexpr locs 'section '() elements))

(define-unary/loc (section* locs . elements)
  (rxexpr locs 'section* '() elements))

(define-unary/loc (subsection locs . elements)
  (rxexpr locs 'subsection '() elements))

(define-unary/loc (subsection* locs . elements)
  (rxexpr locs 'subsection* '() elements))

(define-unary/loc (subsubsection locs . elements)
  (rxexpr locs 'subsubsection '() elements))

(define-unary/loc (subsubsection* locs . elements)
  (rxexpr locs 'subsubsection* '() elements))

(define-unary/loc (emph locs . elements)
  (rxexpr locs 'emph '() elements))

(define-unary/loc (textit locs . elements)
  (rxexpr locs 'textit '() elements))

(define-unary/loc (textbf locs . elements)
  (rxexpr locs 'textbf '() elements))

(define-unary/loc (texttt locs . elements)
  (rxexpr locs 'texttt '() elements))

(define-unary/loc (textrm locs . elements)
  (rxexpr locs 'textrm '() elements))

(define-unary/loc (textsf locs . elements)
  (rxexpr locs 'textsf '() elements))

(define-unary/loc (textsc locs . elements)
  (rxexpr locs 'textsc '() elements))

(define-unary/loc (textup locs . elements)
  (rxexpr locs 'textup '() elements))

(define-unary/loc (textsl locs . elements)
  (rxexpr locs 'textsl '() elements))

(define-unary/loc (enumerate locs #:option [option/#f #f] . elements)
  (rxexpr locs 'enumerate (if option/#f `((option ,option/#f)) '()) elements))

(define-unary/loc (itemize locs . elements)
  (rxexpr locs 'itemize '() elements))

(define-unary/loc (item locs . elements)
  (rxexpr locs 'item '() elements))

(define-unary/loc (lquote locs . elements)
  (rxexpr locs 'quote '() elements))

(define-unary/loc (verbatim locs . elements)
  (rxexpr locs 'verbatim '((verbatim #t)) elements))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define the-documentclass-options "")
(define the-documentclass-name "article")
(define (get-documentclass-options) the-documentclass-options)
(define (get-documentclass-name) the-documentclass-name)
(define-unary/loc (documentclass locs maybe-option . rest)
  (define datum-loc (assoc 'datums locs))
  (cond
    [(and datum-loc (cdr datum-loc) (> (srclocplus-span (cdr datum-loc)) 0))
     (set! the-documentclass-options (format "[~a]" maybe-option))
     (set! the-documentclass-name (car rest))]
    [else
     (set! the-documentclass-name maybe-option)])
  '())

(define-unary/loc (usepackage locs opt-or-name . names)
  (define datum-loc (assoc 'datums locs))
  (if (and datum-loc (cdr datum-loc) (> (srclocplus-span (cdr datum-loc)) 0))
      (rxexpr locs 'usepackage `((option ,opt-or-name)) names)
      (rxexpr locs 'usepackage '() (cons opt-or-name names))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handy commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define |\\| (rxexpr '() '|\\| '() '()))
(define noindent (free-literal 'noindent))
(define clearpage (free-literal 'clearpage))
(define newpage (free-literal 'newpage))
(define pagebreak (free-literal 'pagebreak))

(define tiny (free-literal 'tiny))
(define scriptsize (free-literal 'scriptsize))
(define footnotesize (free-literal 'footnotesize))
(define small (free-literal 'small))
(define normalsize (free-literal 'normalsize))
(define large (free-literal 'large))
(define Large (free-literal 'Large))
(define LARGE (free-literal 'LARGE))
(define huge (free-literal 'huge))
(define Huge (free-literal 'Huge))

(define-unary/loc ($ locs . elements)
  (rxexpr locs '$ '((verbatim #t)) elements))

(define-unary/loc ($$ locs . elements)
  (rxexpr locs '$$ '((verbatim #f)) elements))

(define-unary/loc (center locs . elements)
  (rxexpr locs 'center '() elements))

(define-unary/loc (vspace locs . elements)
  (rxexpr locs 'vspace '() elements))

(define-unary/loc (hspace locs . elements)
  (rxexpr locs 'hspace '() elements))

(define-binary/loc ((renewcommand locs name) locs2 . commands)
  (rxexpr locs2 'renewcommand `((param ,name)) commands))

(define-binary/loc ((setlength locs name) locs2 . num)
  (rxexpr locs 'setlength `((param ,name)) num))

(define-binary/loc ((setcounter locs name) locs2 . num)
  (rxexpr locs 'setcounter `((param ,name)) num))
