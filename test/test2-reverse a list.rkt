#lang racket
(define (reverse-lst lst)
  (define (rev lst1 lst2)
    (if (null? lst1)
        lst2
        (rev (cdr lst1) (cons (car lst1) lst2))))
  (rev lst '()))

(define (loop)
  (let ((a (read)))
     (if (eq? a eof)
         (void)
         (begin
           (displayln (reverse-lst a))
           (loop)))))

(loop)
;runtime error?
;*******another way to solve this problem
#lang racket
(define (reverse-lst lst)
  (if (null? lst)
      '()
      (let ((x (car lst)))
        ;(if (list? x)
            ;(append (reverse-lst (cdr lst)) (list x))
            (append (reverse-lst (cdr lst)) (list x))))
  
  )

(define (loop)
  (let ((a (read)))
     (if (eq? a eof)
         (void)
         (begin
           (displayln (reverse-lst a))
           (loop)))))

(loop)