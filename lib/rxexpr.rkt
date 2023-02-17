#lang racket/base

(require racket/list
         racket/match

         "../rxexpr.rkt"
         "../top.rkt"
         "../srclocplus.rkt")

(provide atomic-printable-value?
         remove-nonprintable-value
         stringify-rxexpr
         insert-newlines-by-source-location
         remove-newlines-by-source-location)

(define (atomic-printable-value? val)
  (or (string? val) (number? val) (symbol? val) (free-literal? val)))

(define (remove-nonprintable-value val)
  (match val
    [(? atomic-printable-value?)
     val]
    [(? list?)
     (for/list ([elem (in-list val)])
       (remove-nonprintable-value elem))]
    [(rxexpr locs tag attrs elems)
     (rxexpr locs tag attrs (remove-nonprintable-value elems))]
    [_
     ""]))

(define (stringify-rxexpr x)
  (match x
    [(? string? s) s]
    [(or (? symbol? x) (? number? x)) (format "~a" x)]
    [(? free-literal? l) l]
    [(rxexpr locs tag attrs elements)
     (cond
       [(attr-ref attrs 'verbatim (λ () #f))
        x]
       [else
        (define new-elements
          (for/list ([e (in-list elements)])
            (stringify-rxexpr e)))
        (rxexpr locs tag attrs new-elements)])]))

(define (insert-newlines-by-source-location x)
  (match x
    [(? atomic-printable-value? v) v]
    [(rxexpr locs tag attrs elements)
     (define new-elements
       (for/list ([e (in-list elements)])
         (insert-newlines-by-source-location e)))
     (define lines-entry (assoc 'lines locs))
     (cond
       [(and lines-entry
             (cdr lines-entry)
             (srclocplus-position (cdr lines-entry))
             (srclocplus-line (cdr lines-entry))
             (srclocplus-line-span (cdr lines-entry))
             (srclocplus-column (cdr lines-entry))
             (srclocplus-next-column (cdr lines-entry)))
        (define lines-loc (and lines-entry (cdr lines-entry)))
        (if (and (>= (srclocplus-line-span lines-loc) 1)
                 (< (sub1 (srclocplus-next-column lines-loc)) (srclocplus-column lines-loc)))
            (rxexpr locs tag attrs (append new-elements '("\n")))
            (rxexpr locs tag attrs new-elements))]
       [else
        (rxexpr locs tag attrs new-elements)])]))

(define (remove-newlines-by-source-location x)
  (match x
    [(? atomic-printable-value? v) v]
    [(rxexpr locs tag attrs elements)
     (define new-elements
       (for/list ([e (in-list elements)])
         (remove-newlines-by-source-location e)))
     (define lines-entry (assoc 'lines locs))
     (cond
       [(and lines-entry
             (cdr lines-entry)
             (srclocplus-position (cdr lines-entry))
             (srclocplus-line (cdr lines-entry))
             (srclocplus-line-span (cdr lines-entry))
             (srclocplus-column (cdr lines-entry))
             (srclocplus-next-column (cdr lines-entry)))
        (define lines-loc (and lines-entry (cdr lines-entry)))
        (if (or (< (srclocplus-line-span lines-loc) 1)
                (>= (sub1 (srclocplus-next-column lines-loc)) (srclocplus-column lines-loc))
                (pair? new-elements) (regexp-match? #rx"\n+" (last new-elements)))
            (rxexpr locs tag attrs (drop-right new-elements 1))
            (rxexpr locs tag attrs new-elements))]
       [else
        (rxexpr locs tag attrs new-elements)])]))
