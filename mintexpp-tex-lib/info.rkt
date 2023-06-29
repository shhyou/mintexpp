#lang info

(define collection 'multi)
(define build-deps '())
(define deps
  '("base"
    "commonmark-lib"
    "syntax-color-lib"
    "scribble-lib"
    "mintexpp-reader-lib"
    "mintexpp-lib"))

(define pkg-desc "TeX backend of the Minimal Tex Preprocessor")
(define version "0.1")
(define pkg-authors '(shhyou))
(define license '(Apache-2.0 OR MIT))
