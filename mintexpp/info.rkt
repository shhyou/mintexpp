#lang info

(define collection 'multi)
(define deps
  '("mintexpp-reader-lib"
    "mintexpp-lib"
    "mintexpp-tex-lib"))
(define implies
  '("mintexpp-reader-lib"
    "mintexpp-lib"
    "mintexpp-tex-lib"))

(define pkg-desc "Minimal Tex Preprocessor, inspired by Pollen")
(define version "0.2")
(define pkg-authors '(shhyou))
(define license '(Apache-2.0 OR MIT))
