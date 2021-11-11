#lang racket
(define (reverse-lst lst)
  (define (rev lst1 lst2)
    (if (null? lst1)
        lst2
        (rev (cdr lst1) (cons (car lst1) lst2))))
  (rev lst '()))

(define (no-rep lst)
  (cond ((null? lst) lst)
        ((null? (cdr lst)) lst)
        ((equal? (car lst) (cadr lst)) (no-rep (cdr lst)))
        (else
         (cons (car lst) (no-rep (cdr lst))))))

(define (power-set lst n)
  (define (find-power f-lst ans);f-lst的P(A)记做ans
    (cond ((null? f-lst) (cons '() ans))
          ;((number? (car f-lst)) ans)
          (else
           (find-power (cdr f-lst) (cons (list (car f-lst)) ans)))
          
          ))
  (if (= n 0)
      (reverse-lst (no-rep lst))
      ;(sort lst max)
      (power-set (find-power (reverse-lst (no-rep lst)) (list (reverse-lst (no-rep lst)))) (- n 1))))

(define (myloop)
  (let ((lst (read))
        (n (read)))
    (if (eq? lst eof)
        (void)
        (begin
          (displayln (power-set (sort (no-rep lst) >) n))
          (myloop)))))
(myloop)