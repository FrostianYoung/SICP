#lang racket

(define (fast-expt a b n);a-b^n b-基数 n-指数
  (cond ((= n 1 ) (* a b))
        ((even? n) (fast-expt a (* b b) (/ n 2)))
        (else (fast-expt (* a b) (* b b) (/ (- n 1) 2)))))
(define (loop)
  (let (( b (read))
        ( n (read)))
    (if (eq? b eof)
        (void)
        (begin (displayln (fast-expt 1 b n)) (loop)))))