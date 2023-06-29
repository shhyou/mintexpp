#lang info

(define collection 'multi)
(define build-deps '())
(define deps
  '("base"
    "syntax-color-lib"
    "scribble-lib"
    "mintexpp-reader-lib"))

(define pkg-desc "Implementation of the Minimal Tex Proprocessor")
(define version "0.1")
(define pkg-authors '(shhyou))
(define license '(Apache-2.0 OR MIT))
