#lang racket

(define exit #f)

(define cont-list '())
(define len (read))

(define (set-cont-list n)
  ;code begin
  (define (op x)
    (if (= x 0)
        (void)
        (begin
          (displayln x)
          (op (- x 1)))))
  (if (= n 0)
      '()
      (set! cont-list
            (cons (call/cc (lambda (m) (op (read)) m))
                   (set-cont-list (- n 1)))))
  
  )
;code end
  
(define (show n);k
  (define (show-helper l n)
    (if (= n 0)
        (if (continuation? (car l))
            ((car l) #f)
            (displayln "error"))
        (show-helper (cdr l) (- n 1))))
  (show-helper cont-list (- n 1)))

(define (main)
  (set-cont-list len)
  (define k (read))
  (if (eq? k eof)
      (void)
      (show k)))

(main)