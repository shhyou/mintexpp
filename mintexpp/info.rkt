#lang info

(define collection 'multi)
(define build-deps '())
(define deps
  '("mintexpp-reader-lib"
    "mintexpp-lib"
    "mintexpp-tex-lib"))
(define update-implies
  '("mintexpp-reader-lib"
    "mintexpp-lib"
    "mintexpp-tex-lib"))

(define pkg-desc "Minimal Tex Proprocessor, inspired by Pollen")
(define version "0.1")
(define pkg-authors '(shhyou))
(define license '(Apache-2.0 OR MIT))
