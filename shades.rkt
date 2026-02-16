#lang racket

(provide create-tints)

(require "color-helper.rkt"
         racket/fixnum
         raylib/2d/unsafe)

(define (create-tints colors tints-count)
  (let ((tint-fraction (fxquotient #xFF (add1 tints-count))))
    (displayln tint-fraction)
    (for/list ((component (range tint-fraction #xFF tint-fraction)))
      (displayln component)
      (let ((blend-color (make-Color component component component #xFF)))
        (for/list ((color colors))
          (color+ color blend-color))))))
