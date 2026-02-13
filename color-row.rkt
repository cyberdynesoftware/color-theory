#lang racket

(provide color-row)

(require "color-helper.rkt")

(define (color-row base-colors fidelity)
  (let loop ((colors base-colors)
             (counter fidelity))
    (if (= counter 0)
        colors
        (loop (let ((new-colors (add-between-f colors color+)))
                (reverse
                  (cons (color+ (first new-colors) (last new-colors))
                        (reverse new-colors))))
              (sub1 counter)))))

(define (add-between-f lst f)
  (flatten
    (cons (first lst)
          (for/list ((pair (partition-clj lst 2 1)))
            (list (apply f pair) (cadr pair))))))

(define (partition-clj lst size (step size))
  (let loop ((result '())
             (remaining lst))
    (if (< (length remaining) size)
        (reverse result)
        (loop (cons (take remaining size) result)
              (drop remaining step)))))
