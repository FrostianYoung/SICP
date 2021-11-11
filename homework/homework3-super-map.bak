#lang racket
    (define (map op lst)
      (if (null? lst)
          '()
          (cons (op (car lst))
                (map op (cdr lst)))))
      
    (define (super-map op . w)

      (define (operations lst ans)
        (if (null? (car lst))
            ans
            (operations (map cdr lst)
                        (append ans (list (apply op (map car lst)))))))
      (operations w '()))

    (define (myloop)
      (let ((a (read))
            (b (read))
            (c (read)))
        (if (eq? a eof)
            (void)
            (begin (displayln (super-map + a b c)) 
                   (displayln (super-map (lambda (x y) (+ x (* 2 y) )) a b ))
                   (myloop)))))
    (myloop)