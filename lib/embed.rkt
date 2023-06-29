#lang racket/base

(require racket/match
         racket/list
         racket/contract

         racket/format

         "../rxexpr.rkt"
         "../top.rkt")

(provide
 current-embed-tag
 current-embed-tag-regexp

 current-embedding
 embed-new-value!
 embed-id-ref

 embed-stringified-rxexpr
 unembed-rxexprs-in-string)

;; tag embedding library
(define current-embed-tag
  (make-parameter
   (apply string-append
          "◇mtpp"
          (for/list ([i (in-range 3)])
            (~r (random (* 36 36))
                #:base '(up 36)
                #:min-width 2
                #:pad-string "0")))))

;; TODO FIXME: memory leak
(define current-embed-tag-hash (make-hash))
(define (current-embed-tag-regexp)
  (define tag (current-embed-tag))
  (hash-ref! current-embed-tag-hash
             (current-embed-tag)
             (λ ()
               (regexp (string-append (regexp-quote (current-embed-tag)) "[0-9]+ff")))))

(define/contract current-embedding
  (parameter/c (hash/c exact-nonnegative-integer? (or/c rxexpr? free-literal?)))
  (make-parameter (make-hash)))

(define (embed-id-ref embedding tag)
  (hash-ref embedding
            (string->number
             (substring tag
                        (string-length (current-embed-tag))
                        (- (string-length tag) 2)))))

(define (embed-new-value! val [embedding-cache #f] [try-cache? #f])
  (define h (current-embedding))
  (cond
    [(and embedding-cache try-cache? (hash-has-key? embedding-cache val))
     (hash-ref embedding-cache val)]
    [else
     (define tag-id (hash-count h))
     (define tag (format "~a~aff" (current-embed-tag) tag-id))
     (hash-set! h tag-id val)
     (when (and embedding-cache try-cache?)
       (hash-set! embedding-cache val tag))
     tag]))

(define (can-cache? x)
  (match x
    [(? free-literal?) #t]
    [(rxexpr _ _ '() '()) #t]
    [_ #f]))

(define (embed-stringified-rxexpr x #:stop? [stop? (λ (x) #f)])
  (define embedding-cache (make-hash))
  (define (do-embed-stringified-rxexpr x)
    (match x
      [(? string? s) s]
      [(? free-literal? l)
       (define embed-id
         (embed-new-value! l embedding-cache (can-cache? l)))
       embed-id]
      [(rxexpr locs tag attrs elements)
       (define new-rxexpr
         (cond
           [(stop? x) x]
           [else
            (define new-elements
              (for/list ([e (in-list elements)])
                (do-embed-stringified-rxexpr e)))
            (rxexpr locs tag attrs new-elements)]))
       (define embed-id
         (embed-new-value! new-rxexpr embedding-cache (can-cache? new-rxexpr)))
       embed-id]))
  (do-embed-stringified-rxexpr x))

;; string to rxexpr-elements
(define (unembed-rxexprs-in-string s)
  (define unembed-cache (make-hash))

  (define (do-unembed-rxexpr rx)
    (match-define (rxexpr locs tag attrs new-elements)
      rx)
    (define unembedded-elements
      (append*
       (for/list ([sub-x (in-list new-elements)])
         (match sub-x
           [(? string? str) (do-unembed-rxexprs-in-string str)]
           [(? free-literal? lit) (list lit)]
           [(? rxexpr? rx) (list (do-unembed-rxexpr rx))]))))
    (rxexpr locs tag attrs unembedded-elements))

  (define (do-unembed-rxexprs-in-string s)
    (define embedding (current-embedding))
    (define p (current-embed-tag-regexp))
    (define all-tags
      (regexp-match-positions* p s))
    (let loop ([next-tag all-tags]
               [start-pos 0])
      (cond
        [(pair? next-tag)
         (define match-interval (car next-tag))
         (define embed-id (substring s (car match-interval) (cdr match-interval)))
         (define prefix
           (cond [(= start-pos (car match-interval)) '()]
                 [else (list (substring s start-pos (car match-interval)))]))
         (define embedded-val
           (embed-id-ref embedding embed-id))
         (define unembedded-val
           (match embedded-val
             [(? free-literal? l) l]
             [_
              #:when (hash-has-key? unembed-cache embed-id)
              (hash-ref unembed-cache embed-id)]
             [(? rxexpr? rx)
              (define unembedded-val
                (do-unembed-rxexpr rx))
              (hash-set! unembed-cache embed-id unembedded-val)
              unembedded-val]))
         (append prefix
                 (list unembedded-val)
                 (loop (cdr next-tag) (cdr match-interval)))]
        [else
         (if (< start-pos (string-length s))
             (list (substring s start-pos))
             '())])))
  (do-unembed-rxexprs-in-string s))
