#lang racket

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))
(define (filter proc seq)
  (cond ((null? seq) '())
        ((proc (car seq)) (cons (car seq) (filter proc (cdr seq))))
        (else (filter proc (cdr seq)))))
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))

(define (LIS lst);LIS
  (define (lis-lenth f-lst k);前k个数的最长上升子序列
    ()))
(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin
          (displayln (LIS a));longest-increasing-subsequence
          (myloop)))))

(myloop)