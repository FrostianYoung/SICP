#lang racket
(define (filter f x);筛选
  (let recur ((x x))
    (if (null? x)
        '()
        (if (f (car x))
            (cons (car x) (recur (cdr x)))
            (recur (cdr x ))))))
(define (merge x y z )
  (let recur ((x x))
    (if (null? x )
        (cons y z)
        (cons (car x) (recur (cdr x))))))
(define (quick-sort x)
  (if (null? x)
      '()
      (if (null? (cdr x))
          x
          (merge (quick-sort (filter (lambda (i) (< i (car x))) (cdr x)))
                 (car x)
                 (quick-sort (filter (lambda (i) (> i (car x))) (cdr x)))))))

(define (read-line)
  (define (read-iter lst)
    (let ((a (read)))
      (if (eq? a eof);ret是输入的数组
          (quick-sort lst)
          (read-iter (cons a lst)))))
  (read-iter '()))

(define (print-line lst)
  (define (print-recur lst)
    (if (null? lst)
        (void)
        (begin (display " ") (display (car lst)) (print-recur (cdr lst)))))
  (begin (display (car lst)) (print-recur (cdr lst))))

