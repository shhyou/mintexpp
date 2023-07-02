#lang racket/base

(provide syntax-srcloc)

(define (syntax-srcloc-adapter stx)
  (and (syntax-position stx)
       (syntax-span stx)
       (make-srcloc (syntax-source stx)
                    (syntax-line stx)
                    (syntax-column stx)
                    (syntax-position stx)
                    (syntax-span stx))))

(define syntax-srcloc
  (with-handlers ([exn:fail:filesystem:missing-module? (λ (e)
                                                         syntax-srcloc-adapter)])
    ;; racket/syntax-srcloc is added in Racket 8.3
    (dynamic-require 'racket/syntax-srcloc 'syntax-srcloc)))
