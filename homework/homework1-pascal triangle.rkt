#lang racket

(define (lst-add a b lst);递归求lst
  (if (null? a)
      lst;返回lst
      (lst-add (cdr a) (cdr b) (append lst (list (+ (car a) (car b)))))))

(define (print-line this-lst)
  (if (null? this-lst)
      (newline)
      (begin (display (car this-lst)) (display " ") (print-line (cdr this-lst)))))

(define (print-pascal-triangle a);pring from t[1]->t[a]
  (define (get-line i lst);print t[i]
    (let ((this-lst (list)));设置一个列表this-lst
      (if (= i 1);第一行
          (set! this-lst '(1));1
          (set! this-lst (lst-add (append lst (list 0));内层递归求lst，左移
                                  (append (list 0) lst);右移，上下对应，直接加和
                                  '())))
      (print-line this-lst)
      (if (> i a)
          (void)
          (get-line (+ i 1) this-lst))))
  (get-line 1 '()));从第一行起，开始递归


(define (loop);test cases
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (print-pascal-triangle (- a 1))
               (loop)))))