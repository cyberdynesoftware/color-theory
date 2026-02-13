#lang racket

(provide dim-color
         color->values
         color+
         color/list+)

(require raylib/2d/unsafe)

(define (dim-color r g b brightness)
  (let ((factor (/ (max r g b) brightness)))
    (make-Color (min #xFF (truncate (/ r factor)))
                (min #xFF (truncate (/ g factor)))
                (min #xFF (truncate (/ b factor)))
                #xFF)))

(define (color->values color)
  (values (Color-r color) (Color-g color) (Color-b color))) 

(define (color->list color)
  (list (Color-r color) (Color-g color) (Color-b color))) 

(define (color+ c1 c2)
  (color/list+ (color->list c1) (color->list c2)))

(define (color/list+ lst1 lst2)
  (let* ((r (+ (first lst1) (first lst2)))
         (g (+ (second lst1) (second lst2)))
         (b (+ (third lst1) (third lst2)))
         (max1 (apply max lst1))
         (max2 (apply max lst2)))
    (dim-color r g b (/ (+ max1 max2) 2))))
