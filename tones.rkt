#lang racket

(provide dim-color)

(require raylib/2d/unsafe)

(define (dim-color r g b brightness)
  (let ((factor (/ (max r g b) brightness)))
    (make-Color (min #xFF (truncate (/ r factor)))
                (min #xFF (truncate (/ g factor)))
                (min #xFF (truncate (/ b factor)))
                #xFF)))
