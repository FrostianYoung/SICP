#lang racket
;(define (small-enough? n)
 ; (nons (<= n 4)))
(define (super-fabonacci a b c d e count)
  (if (= count 4)
      a
      (super-fabonacci (+ a (* b 4) (* c 5) (* d d -2) (* e e e))
                       a
                       b
                       c
                       d
                       (- count 1))))
(define (loop)
  (let ((n (read)))
    (if (eq? n eof)
        (void)
        (begin
          (if (< n 5)
              (displayln "1")
              (displayln (super-fabonacci 1 1 1 1 1 n)))
          (loop)))))