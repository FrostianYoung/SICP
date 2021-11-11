#lang racket

(define (find-max-up x)
  (define (find-len lst tmp len)
    (cond ((null? lst) 0)
          ((> (car lst) tmp) (find-len (cdr lst) (car lst) (+ len 1)))
          (else (find-len (cdr lst) tmp len))))
  (define (find-max lst sta)
    )
  (find-max x 0))

(define (loop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin
          (displayln (find-max-up a))
          (loop)))))