#lang racket

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (filter proc seq)
  (cond ((null? seq) '())
        ((proc (car seq))
         (cons (car seq) (filter proc (cdr seq))))
        (else
         (filter proc (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ 1 a ) b))))

(define empty-board '())
(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))
(define (safe? k position)
  (define (safe?-iter row-of-new-queen rest-of-queens i)
    (if (null? rest-of-queens)
        #t
        (let ((row-of-current-queen (car rest-of-queens)))
          (if (or (= row-of-new-queen row-of-current-queen)
                  (= row-of-new-queen (+ row-of-current-queen i))
                  (= row-of-new-queen (- row-of-current-queen i)))
              #f
              (safe?-iter row-of-new-queen (cdr rest-of-queens) (+ i 1))))))
  (safe?-iter (car position) (cdr position) 1))

(define (queens board-size)
  (define (queen-cols k)
    (if (= 0 k)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;(queens 8)

(define (print-queens b)
  (define (pri-q b lt);从queens8里找出第b个解
    (if (= b 1)
        (car lt)
        (pri-q (- b 1) (cdr lt))))
  (define lst (queens 8))
  (define (pri-ans i f-lst ans);将第b个解(已翻转)以正整数形式输出
    (if (= i 0)
        ans
        (pri-ans (- i 1) (cdr f-lst) (+ (car f-lst) (* 10 ans)))))
  (display (pri-ans 8 (reverse-lst (pri-q b lst)) 0)))

(define (reverse-lst lst);翻转一张表
  (define (rev lst1 lst2)
    (if (null? lst1)
        lst2
        (rev (cdr lst1) (cons (car lst1) lst2))))
  (rev lst '()))

(define (myloop)
  (define (loop a)
    (if (= a 0)
        (void)
        (let ((b (read)))
          (print-queens b)
          (newline)
          (loop (- a 1)))))
  (let ((n (read)))
    (if (= n 0)
        (void)
        (loop n))))

(myloop)