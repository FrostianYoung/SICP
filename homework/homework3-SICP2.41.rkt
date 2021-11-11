#lang racket

    (define (accumulate op init seq)
      (if (null? seq)
          init
          (op (car seq) (accumulate op init (cdr seq)))))

    (define (enumerate-interval a b);枚举算法
      (if (> a b)
          '()
          (cons a (enumerate-interval (+ a 1) b))))

    (define (flatmap proc seq);平摊
      (accumulate append '() (map proc seq)))


(define (tri-num-list n s)
;code begin
  (define (eq-s?)
    (lambda (a)
    (if (= s (+ (car a) (cadr a) (caddr a)))
        #t
        #f)))
  (let ((a
         (flatmap (lambda (i)
                    (flatmap (lambda (j)
                               (map (lambda (k)
                                          (list i j k))
                                    (enumerate-interval (+ j 1) n)))
                             (enumerate-interval (+ i 1) n)))
                  (enumerate-interval 1 n))))
    (filter (eq-s?) a)))

;code end

    (define (myloop)
      (let ((n (read))
            (s (read)))
        (if (eq? n eof)
            (void)
            (begin (display (tri-num-list n s)) (newline) (myloop)))))

    (myloop)