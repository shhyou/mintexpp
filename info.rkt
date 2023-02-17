#lang info

(define collection "mintexpp")
(define build-deps
  '("at-exp-lib"
    "scribble-lib"))
(define deps
  '("base"
    "syntax-color-lib"
    "commonmark-lib"))

(define raco-commands
  '(("mtpp" (submod mintexpp/tools main) "invoke Minimal Tex Preprocessor" #f)))

(define pkg-desc "Minimal Tex Proprocessor, inspired by Pollen")
(define version "0.0")
(define pkg-authors '(shhyou))
(define license '(Apache-2.0 OR MIT))
