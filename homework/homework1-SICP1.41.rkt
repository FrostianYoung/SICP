#lang racket
(define (inc x) (+ x 1));x+1
(define (square x ) (* x x));x^2
(define (doubleF f);f(x)
  (lambda (x) (f (f x))))

((doubleF square) 10);f(x^2)
(define X (doubleF (doubleF doubleF)));f(f(f(x)))
((X inc) 5);f(f(f(x+1)))
(((doubleF (doubleF (doubleF doubleF))) inc) 5) ;f(f(f(f(5)))+1)=261 

(display "********") (newline)
(define (myloop)
  (let ((k (read)))
    (if (eq? k eof)
        (void)
        (begin (display ((X inc) k)) 
               (newline) (myloop)))))