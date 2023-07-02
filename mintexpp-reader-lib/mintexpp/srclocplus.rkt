#lang racket/base

(require racket/syntax-srcloc)

(provide (struct-out srclocplus)
         (struct-out srclocdiff)

         srcloc+diff->srclocplus
         syntax+diff->srclocplus

         srclocplus->diff
         srclocplus->syntax
         srclocplus->syntax+diff)

(struct srclocplus (source line column line-span next-column position span) #:prefab)
(struct srclocdiff (line-span next-column) #:prefab)

(define (srcloc+diff->srclocplus loc diff)
  (srclocplus (srcloc-source loc)
              (srcloc-line loc)
              (srcloc-column loc)
              (srclocdiff-line-span diff)
              (srclocdiff-next-column diff)
              (srcloc-position loc)
              (srcloc-span loc)))

(define (syntax+diff->srclocplus stx diff)
  (srcloc+diff->srclocplus (syntax-srcloc stx) diff))

(define (srclocplus->srcloc loc+)
  (srcloc (srclocplus-source loc+)
          (srclocplus-line loc+)
          (srclocplus-column loc+)
          (srclocplus-position loc+)
          (srclocplus-span loc+)))

(define (srclocplus->diff loc+)
  (srclocdiff (srclocplus-line-span loc+) (srclocplus-next-column loc+)))

(define (srclocplus->syntax loc+ #:context [context-stx #f])
  (define srcloc (srclocplus->srcloc loc+))
  (datum->syntax context-stx
                 'locplus
                 `#(,(srcloc-source srcloc)
                    ,(srcloc-line srcloc)
                    ,(srcloc-column srcloc)
                    ,(srcloc-position srcloc)
                    ,(srcloc-span srcloc))))

(define (srclocplus->syntax+diff loc+ #:context [context-stx #f])
  (values (srclocplus->syntax loc+ #:context context-stx)
          (srclocplus->diff loc+)))
