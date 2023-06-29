#lang racket

(require mintexpp
         racket/pretty
         racket/list)

(provide (all-defined-out))

(define (complete-and-simplify-path path)
  (simplify-path (path->complete-path path)))

(define (render-file file-name
                     #:cache? [cache? #f]
                     #:quiet? [quiet? #t])
  (define output-file
    (path-replace-extension file-name (dynamic-require file-name 'extension)))
  (define-values (base name dir?)
    (split-path file-name))
  (define zo-file
    (build-path base "compiled" (path-replace-extension name "_rtex.zo")))
  (define doc ((dynamic-require 'mintexpp 'load-document) file-name))
  (cond
    [(and cache?
          (file-exists? output-file)
          (>= (file-or-directory-modify-seconds output-file)
              (file-or-directory-modify-seconds file-name))
          (or (not (file-exists? zo-file))
              (>= (file-or-directory-modify-seconds output-file)
                  (file-or-directory-modify-seconds zo-file))))
     (when (not quiet?)
       (printf " [already up-to-date at ~s]\n"
               (path->string (find-relative-path base output-file))))]
    [doc
     (define render-from-template
       (dynamic-require file-name 'render-from-template))
     (define rendered-result
       (with-handlers ([exn:fail? (λ (e)
                                    (define new-error-message
                                      (format "Exception raised when rendering ~a\n message: ~a"
                                              file-name
                                              (exn-message e)))
                                    ((error-display-handler)
                                     new-error-message
                                     (exn:fail new-error-message (exn-continuation-marks e)))
                                    "")])
         (render-from-template doc)))
     (with-output-to-file output-file #:exists 'truncate
       (λ ()
         (let loop ([rendered-result rendered-result])
           (cond
             [(string? rendered-result)
              (write-string rendered-result)]
             [(null? rendered-result)
              (void)]
             [(pair? rendered-result)
              (loop (car rendered-result))
              (loop (cdr rendered-result))]))))]
    [else (void)]))

(module* main #f
  (require racket/cmdline
           raco/command-name)

  (define-namespace-anchor ns-here)

  (define *cache*? #f)
  (define *load-errortrace? #f)

  (define file-names
    (command-line
     #:program (short-program+command-name)
     #:once-each
     ["--cache" "Skip rendered files" (set! *cache*? #t)]
     ["--errortrace" "Load errortrace" (set! *load-errortrace? #t)]
     #:args file1
     (if (not (null? file1))
         file1
         (for/list ([file (in-list (directory-list "."))]
                    #:when (file-exists? file)
                    #:when (path-has-extension? file #".rtex"))
           file))))

  (define doc-errs
    (append*
     (reverse
      (let loop ([rendered-files (set)]
                 [file-names file-names]
                 [resolved-file-names (map complete-and-simplify-path file-names)]
                 [rev-doc-errss '()])
        (cond
          [(null? file-names) rev-doc-errss]
          [(set-member? rendered-files (car resolved-file-names))
           (loop rendered-files
                 (cdr file-names)
                 (cdr resolved-file-names)
                 rev-doc-errss)]
          [else
           (define resolved-file-name (car resolved-file-names))
           (define-values (doc-errs relative-extra-deps)
             (parameterize ([current-namespace (make-base-empty-namespace)])
               (namespace-attach-module (namespace-anchor->namespace ns-here) 'racket)
               (when *load-errortrace?
                 (dynamic-require 'errortrace #f))
               (printf "Rendering ~a\n" (car file-names))
               ((dynamic-require 'mintexpp 'clear-extra-dependencies!))
               (render-file resolved-file-name #:quiet? #f #:cache? *cache*?)
               (values ((dynamic-require 'mintexpp 'get-document-errors))
                       ((dynamic-require 'mintexpp 'get-extra-dependencies)))))
           (define-values (base-path name dir?)
             (split-path resolved-file-name))
           (define extra-deps
             (for/list ([relative-dep (in-list relative-extra-deps)])
               (if (relative-path? relative-dep)
                   (build-path base-path relative-dep)
                   relative-dep)))
           (loop (set-add rendered-files resolved-file-name)
                 (append extra-deps (cdr file-names))
                 (append (map complete-and-simplify-path extra-deps) (cdr resolved-file-names))
                 (cons doc-errs rev-doc-errss))])))))
  (unless (null? doc-errs)
    (report-document-errors doc-errs)
    (exit 1))
  )
