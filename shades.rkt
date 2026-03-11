#lang racket

(provide create-tints)

(require "color-helper.rkt"
         "color-row.rkt"
         racket/fixnum
         raylib/2d/unsafe)

(define (create-tints colors tints-iterations)
  (if (= tints-iterations 0)
    empty
    (let loop ((result (list (blend-colors colors WHITE)))
               (counter (sub1 tints-iterations)))
      (if (= counter 0)
          result
          (let ((new-rows (blend-colors-between result)))
            (loop (prepend-color-row 
                    (append-color-row new-rows WHITE)
                    colors)
                  (sub1 counter)))))))

(define (blend-colors colors blend-color)
  (for/list ((color colors))
    (color+ color blend-color)))

(define (blend-colors-between rows)
  (if (= (length rows) 1)
      rows
      (reverse
        (cons (last rows)
              (reverse
                (foldr
                  (lambda (pair result)
                    (cons (car pair)
                          (cons (map color+ (car pair) (cadr pair))
                                result)))
                  '()
                  (partition-clj rows 2 1)))))))

(define (prepend-color-row lst colors)
  (cons (map color+ (car lst) colors)
        lst))

(define (append-color-row lst color)
  (reverse
    (cons (blend-colors (last lst) color)
          (reverse lst))))
