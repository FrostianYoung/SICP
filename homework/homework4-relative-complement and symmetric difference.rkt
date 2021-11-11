#lang racket

(define (no-rep lst);去重
  (cond ((null? lst) lst)
        ((null? (cdr lst)) lst)
        ((= (car lst) (cadr lst)) (no-rep (cdr lst)))
        (else
         (cons (car lst) (no-rep (cdr lst))))))

(define (union-set a b);并集
  (cond ((null? a) b)
        ((null? b) a)
        (else
         (let ((a1 (car a))
               (b1 (car b)))
           (cond ((> a1 b1) (cons b1 (union-set a (cdr b))))
                 ((= a1 b1) (cons a1 (union-set (cdr a) (cdr b))))
                 ((< a1 b1) (cons a1 (union-set (cdr a) b))))))))

(define (relative-complement a b);补集
  (cond ((null? a) '())
        ((null? b) a)
        (else
         (let ((a1 (car a))
               (b1 (car b)))
           (cond ((> a1 b1) (relative-complement a (cdr b)))
                 ((< a1 b1) (cons a1 (relative-complement (cdr a) b)))
                 ((= a1 b1) (relative-complement (cdr a) (cdr b))))))))

(define (symmetric-difference a b);相对补
  (union-set (relative-complement a b) (relative-complement b a)))

(define (myloop)
  (let ((a (read))
        (b (read)))
    (if (eq? a eof)
        (void)
        (let ((a1 (no-rep (sort a <)))
                (b1 (no-rep (sort b <))))
          (begin
            (display (relative-complement a1 b1))
            (display (symmetric-difference a1 b1))
            (newline)
            (myloop))))))

(myloop)