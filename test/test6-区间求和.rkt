#lang racket

(define len '())
(define (set-lr l r x)
  (set! len (cons (list l r x) len)))
(define (min x y)
  (if (< x y)
      x
      y))
(define (max x y)
  (if (> x y)
      x
      y))
(define (sum-lr1 l r lst)
  (let ((ll (car lst))
        (lr (cadr lst))
        (lx (caddr lst)))
    (if (and (>= r ll) (<= l lr))
        (* lx (+ (- (min r lr) (max l ll)) 1))
        0)))
(define (sum-lr l r lst)
  (if (null? lst)
      0
      (+ (sum-lr l r (cdr lst)) (sum-lr1 l r (car lst)))))

(define (myloop)
  (let ((m (read)))
    (define (loop x)
      (if (= x 0)
          (void)
          (let ((op (read)))
              (begin
                (if (= op 0)
                    (displayln (sum-lr (read) (read) len))
                    (set! len (cons (list (read) (read) (read)) len)))
              (loop (- x 1))
              ))))
    (loop m)))

(myloop)