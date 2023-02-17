(module reader syntax/module-reader

  ;; module path
  mintexpp/doc

  #:info mintexpp-info

  #:whole-body-readers? #t

  #:read (λ (inp)
           (port-count-lines! inp)
           (parameterize ([current-readtable (make-base-readtable)])
             (mintexpp-read-inside inp)))

  #:read-syntax (λ (src inp)
                  (port-count-lines! inp)
                  (parameterize ([current-readtable (make-base-readtable)])
                    (mintexpp-read-syntax-inside src inp)))

  (require mintexpp/at-reader)

  (define (mintexpp-info key defval default)
    (case key
      [(color-lexer)
       ((dynamic-require 'syntax-color/scribble-lexer 'make-scribble-inside-lexer))]
      [(drracket:indentation)
       (dynamic-require 'scribble/private/indentation 'determine-spaces)]
      ; [(drracket:range-indentation)
      ;  (dynamic-require 'scribble/private/indentation 'paragraph-indentation)]
      [(drracket:default-filters)
       `(["Article" "*.rtex"])]
      [(drracket:default-extension)
       "rtex"]
      [else
       (default key defval)]))
  )
