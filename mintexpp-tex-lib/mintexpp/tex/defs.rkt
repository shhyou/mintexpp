#lang racket/base

(require mintexpp/rxexpr mintexpp/top mintexpp/at-reader)

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

(define/loc (chapter locs . elements)
  (rxexpr locs 'chapter '() elements))

(define/loc (chapter* locs . elements)
  (@ (free-literal 'phantomsection)
     (rxexpr locs 'chapter* '() elements)
     (apply
      ((rxexpr/app '() 'addcontentsline '() '("toc"))
       '() "chapter")
      '() elements)))

(define/loc (section locs . elements)
  (rxexpr locs 'section '() elements))

(define/loc (section* locs . elements)
  (rxexpr locs 'section* '() elements))

(define/loc (subsection locs . elements)
  (rxexpr locs 'subsection '() elements))

(define/loc (subsection* locs . elements)
  (rxexpr locs 'subsection* '() elements))

(define/loc (subsubsection locs . elements)
  (rxexpr locs 'subsubsection '() elements))

(define/loc (subsubsection* locs . elements)
  (rxexpr locs 'subsubsection* '() elements))

(define/loc (emph locs . elements)
  (rxexpr locs 'emph '() elements))

(define/loc (textit locs . elements)
  (rxexpr locs 'textit '() elements))

(define/loc (textbf locs . elements)
  (rxexpr locs 'textbf '() elements))

(define/loc (texttt locs . elements)
  (rxexpr locs 'texttt '() elements))

(define/loc (textrm locs . elements)
  (rxexpr locs 'textrm '() elements))

(define/loc (textsf locs . elements)
  (rxexpr locs 'textsf '() elements))

(define/loc (textsc locs . elements)
  (rxexpr locs 'textsc '() elements))

(define/loc (textup locs . elements)
  (rxexpr locs 'textup '() elements))

(define/loc (textsl locs . elements)
  (rxexpr locs 'textsl '() elements))

(define/loc (mathit locs . elements)
  (rxexpr locs 'mathit '() elements))

(define/loc (mathbf locs . elements)
  (rxexpr locs 'mathbf '() elements))

(define/loc (mathtt locs . elements)
  (rxexpr locs 'mathtt '() elements))

(define/loc (mathsf locs . elements)
  (rxexpr locs 'mathsf '() elements))

(define-arbitrary-procedure/loc enumerate (free-literal 'enumerate))

(define-arbitrary-procedure/loc itemize (free-literal 'itemize))

(define/loc (item locs . maybe-label-contents)
  (cond
    [(at-exp-datums-loc locs)
     (unless (not (null? maybe-label-contents))
       (error 'item
              (string-append "expected the label of the item, but given no arguments.\n"
                             "  location (if any): ~a")
              (format-locs locs)))
     (rxexpr locs 'item `((datums ,(car maybe-label-contents))) (cdr maybe-label-contents))]
    [else
     (rxexpr locs 'item '() maybe-label-contents)])
  (rxexpr locs 'item '() maybe-label-contents))

(define/loc (lquote locs . elements)
  (rxexpr locs 'quote '() elements))

(define/loc (verbatim locs . elements)
  (rxexpr locs 'verbatim '((markdown? #f)) elements))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/loc ((array locs1 . maybe-align-style) locs2 . elements)
  (define array-with-style
    (cond
      [(at-exp-datums-loc locs1)
       (unless (not (null? maybe-align-style))
         (error 'array
                (string-append "expected array positioning specifier, but "
                               "given no arguments.\n"
                               "  locations (if any): ~a\n"
                               "  other arguments: ~a")
                (format-locs locs1)
                elements))
       (rxexpr/app locs1 'array `((datums ,(car maybe-align-style))) (cdr maybe-align-style))]
      [else
       (rxexpr/app locs1 'array '() maybe-align-style)]))
  (apply array-with-style locs2 elements))

(define/loc (figure locs . maybeopts-content)
  (cond
    [(at-exp-datums-loc locs)
     (unless (not (null? maybeopts-content))
       (error 'figure
              (string-append "expected figure placement specifier, but given no arguments.\n"
                             "  locations (if any): ~a")
              (format-locs locs)))
     (rxexpr locs 'figure `((datums ,(car maybeopts-content))) (cdr maybeopts-content))]
    [else
     (rxexpr locs 'figure '() maybeopts-content)]))

;; Usages:
;;
;;   @minipage{0.65\textwidth}{ ... page content ... }
;;
;;   @minipage["t"]{0.7\textwidth}{ ... page content ... }
;;             position: T, t, c, b
;;
;; Otherwise, use #:pos, #:height and #:inner-pos to produce
;;
;;   \begin{minipage}[pos][height][inner-pos]
(define/loc ((minipage locs1
                       #:pos [pos #f]
                       #:height [height #f]
                       #:inner-pos [inner-pos #f]
                       . maybe-pos-width)
             locs2 . elements)
  (define minipage-with-opts
    (cond
      [(or pos height inner-pos)
       (rxexpr/app locs1 'minipage
                   `((datums ,(string-append
                               (format "~a" (or pos ""))
                               "]["
                               (format "~a" (or height ""))
                               "]["
                               (format "~a" (or inner-pos "")))))
                   maybe-pos-width)]
      [(at-exp-datums-loc locs1) ;; has [ ] but no keyword arguments
       (unless (not (null? maybe-pos-width))
         (error 'minipage
                (string-append "expected minipage positioning specifier, but "
                               "given no arguments.\n"
                               " to specify pos, height and inner-pos, use "
                               " #:pos, #:height, and #:inner-pos.\n"
                               "  locations (if any): ~a\n"
                               "  other arguments: ~a")
                (format-locs locs1)
                elements))
       (rxexpr/app locs1 'minipage `((datums ,(car maybe-pos-width))) (cdr maybe-pos-width))]
      [else
       (rxexpr/app locs1 'minipage '() maybe-pos-width)]))
  (apply minipage-with-opts locs2 elements))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define the-documentclass-options "")
(define the-documentclass-name "article")
(define (get-documentclass-options) the-documentclass-options)
(define (get-documentclass-name) the-documentclass-name)
(define/loc (documentclass locs maybe-option . rest)
  (cond
    [(at-exp-datums-loc locs)
     (set! the-documentclass-options (format "[~a]" maybe-option))
     (set! the-documentclass-name (car rest))]
    [else
     (set! the-documentclass-name maybe-option)])
  '())

(define/loc (usepackage locs opt-or-name . names)
  (if (at-exp-datums-loc locs)
      (rxexpr locs 'usepackage `((datums ,opt-or-name)) names)
      (rxexpr locs 'usepackage '() (cons opt-or-name names))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handy commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define |\\| (free-literal '|\\|))
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

(define/loc ($ locs . elements)
  (rxexpr locs '$ '((markdown? #f)) elements))

(define/loc ($$ locs . elements)
  (rxexpr locs '$$ '((markdown? #f)) elements))

(define/loc (center locs . elements)
  (rxexpr locs 'center '() elements))

(define/loc (vspace locs . elements)
  (rxexpr locs 'vspace '() elements))

(define/loc (hspace locs . elements)
  (rxexpr locs 'hspace '() elements))

(define/loc ((newcommand locs name) locs2 . commands)
  (define newcommand-name
    (rxexpr/app locs 'newcommand '() (list name)))
  (if (at-exp-datums-loc locs2)
      (apply newcommand-name locs2 #:datums (car commands) (cdr commands))
      (apply newcommand-name locs2 commands)))

(define/loc ((renewcommand locs name) locs2 . commands)
  (define renewcommand-name
    (rxexpr/app locs 'renewcommand '() (list name)))
  (if (at-exp-datums-loc locs2)
      (apply renewcommand-name locs2 #:datums (car commands) (cdr commands))
      (apply renewcommand-name locs2 commands)))

(define/loc ((setlength locs name) locs2 . num)
  (apply (rxexpr/app locs 'setlength '() (list name)) locs2 num))

(define/loc ((setcounter locs name) locs2 . num)
  (apply (rxexpr/app locs 'setcounter '() (list name)) locs2 num))
