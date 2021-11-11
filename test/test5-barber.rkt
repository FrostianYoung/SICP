#lang racket
(define (reverse-lst lst)
  (define (rev lst1 lst2)
    (if (null? lst1)
        lst2
        (rev (cdr lst1) (cons (car lst1) lst2))))
  (rev lst '()))

(define (delete n p)
  (cond ((> n p) (- n p))
        (else 0)))

(define (find-seat n m)
  (define (make-seat lst n);建表(x0,x1,...,xn-1)xi表示理发时间,xi=0表示空坐
    (if (= n 0)
        lst
        (make-seat (cons 0 lst) (- n 1))))
  (define (change-seat f-lst t-lst p n);来顾客后p时间，改变列表到t-lst
    (if (= n 0)
        (reverse-lst t-lst)
        (change-seat (cdr f-lst)
                     (cons (delete (car f-lst) p)
                           t-lst)
                     p
                     (- n 1))))
  (define (find-null lst n a);找到空位
    (if (= n a)
        -1
        (if (= (car lst) 0)
            a
            (find-null (cdr lst) n (+ a 1)))))
  (define (change-this f-lst t-lst q a n)
    (if (= n 0)
        (reverse-lst t-lst)
        (if (= a 0)
            (change-this (cdr f-lst)
                         (cons (+ (car f-lst) q)
                               t-lst)
                         q
                         (- a 1)
                         (- n 1))
            (change-this (cdr f-lst)
                         (cons (car f-lst) t-lst)
                         q
                         (- a 1)
                         (- n 1)))))
  (define (seat-in lst q n);坐下
    (let ((x (find-null lst n 0)))
      ;(begin
       ; (displayln x)
      (if (>= x 0);有空位
          (begin
            (displayln x)
            (change-this lst '() q x n))
          (begin
            (displayln "Failed")
            lst))));)
  (let ((t-lst (make-seat '() n)))
    (define (loop lst m)
      (if (= m 0)
          ;(displayln lst)
          (void)
          (let ((p (read))
                (q (read)))
            ;(if (eq? p))
            ;(begin
              ;(displayln lst)
              ;(displayln (change-seat lst '() p n))
              ;(set! lst (change-seat lst '() p (+ n 1)));改变理发坐
               ; (displayln (seat-in lst q n)))
              (loop (seat-in (change-seat lst '() p n) q n) (- m 1)))))
    (loop t-lst m)))

(define (myloop)
  (let ((n (read))
        (m (read)))
    (if (eq? n eof)
        (void)
        (begin
          (find-seat n m)
          (myloop)))))

(myloop)