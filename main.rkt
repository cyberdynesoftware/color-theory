#lang racket

(provide dim/brighten)

(require "base-colors.rkt"
         raylib/2d/unsafe
         racket/fixnum)

(module+ main
  (apply run (vector->list (current-command-line-arguments))))

(define (run (fidelity "5") . args)
  (set-colors (string->number fidelity) args)
  (open-window)
  (let loop ((nix null))
    (handle-input)
    (draw)
    (if (WindowShouldClose)
        (CloseWindow)
        (loop nix))))

(define width 640)
(define height 480)

(define (open-window)
  (InitWindow width height "Color theory")
  (SetTargetFPS 60))

(define (draw)
  (BeginDrawing)
  (ClearBackground WHITE)
  (draw-colors width height)
  (EndDrawing))

(define (draw-colors width height)
  (let ((rects (rectangles width height)))
    (for* ((row rects)
           (cell row))
      (DrawRectangleRec (car cell) (cadr cell)))))

(define colors (list (list WHITE)))

(define (rectangles width height)
  (let ((h (/ height (length colors))))
    (for/list ((y (length colors))
               (row colors))
      (let ((w (/ width (length row))))
        (for/list ((x (length row))
                   (color row))
          (list (make-Rectangle (exact->inexact (* x w))
                                (exact->inexact (* y h))
                                (exact->inexact w)
                                (exact->inexact h))
                color))))))

(define (handle-input)
  (when (IsMouseButtonPressed MOUSE_BUTTON_LEFT)
    (let ((cell (for/or ((row (rectangles width height)))
                  (findf (lambda (item)
                           (CheckCollisionPointRec (GetMousePosition) (car item)))
                         row))))
      (display-color (cadr cell)))))

(define (display-color color)
  (let ((fmt (lambda (s) (~r #:base 16 #:min-width 2 #:pad-string "0" s))))
    (displayln (format "~a~a~a"
                       (fmt (Color-r color))
                       (fmt (Color-g color))
                       (fmt (Color-b color))))))

(define (set-colors fidelity color-list)
  (let ((mycolors (map decode-color-string color-list)))
    (set! colors (color-grid (base-colors mycolors) fidelity))))

(define (decode-color-string color-string)
  (let ((r (string->number (substring color-string 0 2) 16))
        (g (string->number (substring color-string 2 4) 16))
        (b (string->number (substring color-string 4 6) 16)))
    (make-Color r g b #xFF)))

(define (color->values color)
  (values (Color-r color) (Color-g color) (Color-b color))) 

(define (color->list color)
  (list (Color-r color) (Color-g color) (Color-b color))) 

(define (Color+ c1 c2)
  (color+ (color->list c1) (color->list c2)))

(define (color+ lst1 lst2)
  (let* ((r (+ (first lst1) (first lst2)))
         (g (+ (second lst1) (second lst2)))
         (b (+ (third lst1) (third lst2)))
         (max1 (apply max lst1))
         (max2 (apply max lst2)))
    (dim-color r g b (/ (+ max1 max2) 2))))

(define (dim-color r g b brightness)
  (let ((factor (/ (max r g b) brightness)))
    (make-Color (min #xFF (truncate (/ r factor)))
                (min #xFF (truncate (/ g factor)))
                (min #xFF (truncate (/ b factor)))
                #xFF)))

(define (dim/brighten color brightness)
  (let*-values (((color) (decode-color-string color))
                ((r g b) (color->values color)))
    (display-color (dim-color r g b brightness))))

(define (opposites color)
  (let-values (((r g b) (color->values color)))
    (list color
          (make-Color b r g #xFF)
          (make-Color g b r #xFF))))

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

(define (new-color-row previous-color-row)
  (let loop ((remaining previous-color-row)
             (result empty))
    (if (null? remaining)
        (reverse result)
        (loop (rest remaining)
              (let ((other (if (= (length remaining) 1)
                               (first previous-color-row)
                               (second remaining))))
                (cons (Color+ (first remaining) other)
                      (cons (first remaining) result)))))))

(define (color-pyramid first-color-row number-of-rows)
  (let loop ((result (list first-color-row)))
    (if (= (length result) number-of-rows)
        (begin
          (displayln (format "number of colors: ~a" (length (first result))))
          (reverse result))
        (loop (cons (new-color-row (first result)) result)))))

(define (py1 color rows)
  (let-values (((r g b) (color->values color)))
    (set! colors (color-pyramid (opposites color) rows))
    (draw)))

(define (py2 color-list rows)
  (let ((complemented-colors (opposite (first color-list)
                                       (second color-list))))
    (set! colors (color-pyramid complemented-colors rows))
    (draw)))

(define (factors number)
  (let ((x number)
        (y 1))
    (for* ((a number)
           (b (in-range number 0 -1))
           #:when (< a b))
      (when (= (* a b) number)
        (set! x b)
        (set! y a)))
    (values x y)))

(define (color-grid start-colors fidelity)
  (let ((colors (last (color-pyramid start-colors fidelity))))
    (let-values (((columns rows) (factors (length colors))))
      (let loop ((result (list (take colors columns)))
                 (remaining-colors (drop colors columns)))
        (if (empty? remaining-colors)
            (reverse result)
            (loop (cons (take remaining-colors columns) result)
                  (drop remaining-colors columns)))))))

(define (gi1 color fidelity)
  (set! colors (color-grid (opposites color) fidelity))
  (draw))

(define (gi2 color-list fidelity)
  (let ((complemented-colors (opposite (first color-list)
                                       (second color-list))))
    (set! colors (color-grid complemented-colors fidelity))
    (draw)))
