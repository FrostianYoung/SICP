#lang racket
(require r5rs)
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue) (error "DELETE!called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))



(define len (make-queue))
(define visited (make-vector 100010 0))

(define (get-to k);广搜
  (let ((fron (front-queue len)))
    (let ((x (car fron))
          (t (cdr fron)))
      (if (= x k)
          t;抓到
          (begin
            (vector-set! visited x t)
            ;(set! len (make-a-queue))
            (delete-queue! len)
            (if (and (> (- x 1) 0)
                     (= (vector-ref visited (- x 1)) 0))
                (insert-queue! len (cons (- x 1) (+ t 1)))
                (void))
            (if (and (< (+ x 1) 100001)
                     (= (vector-ref visited (+ x 1)) 0))
                (insert-queue! len (cons (+ x 1) (+ t 1)))
                (void))
            (if (and (< (* x 2) 100001)
                     (= (vector-ref visited (* x 2)) 0))
                (insert-queue! len (cons (* x 2) (+ t 1)))
                )
            (get-to k))))))

(define (myloop)
  (let ((N (read))
        (K (read)))
    (if (eq? N eof)
        (void)
        (begin
          (if (< K N)
              (displayln (- N K));倒着走
              (begin
                (insert-queue! len (cons N 0))
                (displayln (get-to K))
                (set! visited (make-vector 100010 0))))
          (myloop)))))

(myloop)