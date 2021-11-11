#lang racket
;code begin

(define (A)
  (define init '());初始化
  
  (define type 'w);w初始,des析构,con构造
  (define (construction x);构造函数
    (if (eq? type 'w)
        (begin (set! init x)
               (set! type 'con))
        (displayln "object has been constructed!")))
  (define (destruction);析构函数
    (if (eq? type 'des)
        (displayln "object has been destructed!")
        (set! type 'des)))
  (define (setx x);接口
    (cond ((eq? type 'w) (displayln "object hasn't been constructed!"))
          ((eq? type 'con) (set! init x))
          ((eq? type 'des) (displayln "object has been destructed！"))))
  (define (getx)
    (cond ((eq? type 'w) (displayln "object hasn't been constructed!"))
          ((eq? type 'con) init)
          ((eq? type 'des) (displayln "object has been destructed!"))))

  (define (function a)
    (cond ((eq? a 'setx) setx)
          ((eq? a 'getx) (getx))
          ((eq? a 'destruction) (destruction))
          (else
           (construction a))))

  function)

(define (delete a)
  (a 'destruction))
;code end

    (define a (A))
    (a 2)
    (a 4)
    (display (a 'getx))
    (newline)
    ((a 'setx) 1)
    (display (a 'getx))
    (newline)
    (define b (A))
    ((b 'setx) 2)
    (b 5)
    (display (b 'getx))
    (newline)
    (delete a)
    (delete b)
    (delete a)
    (b 'getx)