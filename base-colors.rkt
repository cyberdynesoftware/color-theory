#lang racket

(provide base-colors)

(require "tones.rkt"
         raylib/2d/unsafe)

(define (base-colors color-list)
  (case (length color-list)
    ((1) (opposites (car color-list)))
    ((2) (opposite (car color-list) (cadr color-list)))
    ;((3) color-list)
    (else (opposites (make-Color #xFF #x00 #x00 #xFF)))))

(define (opposites color)
  (let-values (((r g b) (color->values color)))
    (list color
          (make-Color b r g #xFF)
          (make-Color g b r #xFF))))

(define (color->values color)
  (values (Color-r color) (Color-g color) (Color-b color))) 

(define (shift-dir index target)
  (let ((dir (- target index)))
    (case dir
      ((2) -1)
      ((-2) 1)
      (else dir))))

(define (list-shift lst dir)
  (case dir
    ((1) (cons (last lst) (drop-right lst 1)))
    ((-1) (append (drop lst 1) (take lst 1)))))

(define (opposite c1 c2)
  (let-values (((r1 g1 b1) (color->values c1))
               ((r2 g2 b2) (color->values c2)))
    (let* ((lst1 (list r1 g1 b1))
           (lst2 (list r2 g2 b2))
           (max1 (max r1 g1 b1))
           (max2 (max r2 g2 b2))
           (index1 (index-of lst1 max1))
           (index2 (index-of lst2 max2))
           (target (car (remove* (list index1 index2) '(0 1 2)))))
      (values (list c1
                    c2 
                    (color+ (list-shift lst1 (shift-dir index1 target))
                            (list-shift lst2 (shift-dir index2 target))))))))

(define (color+ lst1 lst2)
  (let* ((r (+ (first lst1) (first lst2)))
         (g (+ (second lst1) (second lst2)))
         (b (+ (third lst1) (third lst2)))
         (max1 (apply max lst1))
         (max2 (apply max lst2)))
    (dim-color r g b (/ (+ max1 max2) 2))))
