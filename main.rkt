#lang racket

(require "base-colors.rkt"
         "color-row.rkt"
         "shades.rkt"
         raylib/2d/unsafe
         racket/fixnum
         racket/flonum)

(module+ main
  (let ((width 640)
        (height 480)
        (fidelity 2)
        (tints 0)
        (shades 0))
    (command-line
      #:once-each
      (("--size") w h
                  "size of the window in pixels"
                  (set! width (string->number w))
                  (set! height (string->number h)))
      (("--fidelity") it
                      "number of iterations to create colors"
                      (set! fidelity (string->number it)))
      (("--tints") it
                   "number of tints per color"
                   (set! tints (string->number it)))
      (("--shades") it
                    "number of shades per color"
                    (set! shades (string->number it)))
      #:args color-args
      (let* ((input-colors (map decode-color-string color-args))
             (colors (color-row (base-colors input-colors) fidelity))
             (tinted-colors (create-tints colors tints))
             (color-board (create-color-board colors tinted-colors width height)))
        (run width height color-board)))))

(define-struct cell (rect color))

(define (create-color-board colors tints width height)
  (let* ((num-rows (add1 (length tints)))
         (cell-width (fl/ (fx->fl width) (fx->fl (length colors))))
         (cell-height (fl/ (fx->fl height) (fx->fl num-rows))))
    (append
      (create-color-row colors 0 cell-width cell-height)
      (flatten
        (for/list ((shades-row tints)
                   (row-index (range 1 num-rows)))
          (create-color-row shades-row row-index cell-width cell-height))))))

(define (create-color-row colors row-index cell-width cell-height)
  (let ((y (fl* (fx->fl row-index) cell-height)))
    (for/list ((x (length colors))
               (color colors))
      (make-cell
        (make-Rectangle (fl* (fx->fl x) cell-width)
                        y
                        cell-width
                        cell-height)
        color))))

(define (run width height color-board)
  (InitWindow width height "Color theory")
  (SetTargetFPS 60)
  (let loop ((nix null))
    (handle-input color-board)
    (BeginDrawing)
    (ClearBackground WHITE)
    (draw-color-board color-board)
    (EndDrawing)
    (if (WindowShouldClose)
        (CloseWindow)
        (loop nix))))

(define (draw-color-board color-board)
  (for ((cell color-board))
    (DrawRectangleRec (cell-rect cell) (cell-color cell))))

(define (rectangles colors width height)
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

(define (handle-input color-board)
  (when (IsMouseButtonPressed MOUSE_BUTTON_LEFT)
    (let ((hit (findf
                 (lambda (cell)
                   (CheckCollisionPointRec (GetMousePosition) (cell-rect cell)))
                 color-board)))
      (display-color (cell-color hit)))))

(define (display-color color)
  (let ((fmt (lambda (s) (~r #:base 16 #:min-width 2 #:pad-string "0" s))))
    (displayln
      (format "~a~a~a"
              (fmt (Color-r color))
              (fmt (Color-g color))
              (fmt (Color-b color))))))

(define (decode-color-string color-string)
  (let ((r (string->number (substring color-string 0 2) 16))
        (g (string->number (substring color-string 2 4) 16))
        (b (string->number (substring color-string 4 6) 16)))
    (make-Color r g b #xFF)))

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
  (let ((colors start-colors));(last (color-pyramid start-colors fidelity))))
    (let-values (((columns rows) (factors (length colors))))
      (let loop ((result (list (take colors columns)))
                 (remaining-colors (drop colors columns)))
        (if (empty? remaining-colors)
            (reverse result)
            (loop (cons (take remaining-colors columns) result)
                  (drop remaining-colors columns)))))))
