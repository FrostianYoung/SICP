#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your count-pairs, the program in the input can use count-pairs
 
;code begin
 '(define (count-pairs x)
  (define (in-lst x lst)
    (if (and (pair? x) (not (memq x lst)))
        (in-lst (car x) (in-lst (cdr x) (cons x lst)))
        lst))
   (length (in-lst x '())))
;code end
     env)

    (define (myloop)
      (define (eval-codes codes last-val)
        (if (null? codes)
            last-val
            (eval-codes (cdr codes) (eval (car codes) env))))
        
      (let ((codes (read)))
        (if (eq? codes eof)
            (void)
            (begin (displayln (eval-codes codes (void))) (myloop)))))


    (myloop)

