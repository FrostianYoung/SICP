#lang racket

(define (sum-odd-tree tree)
  (define (tree-line tree-tmp line)
    (cond ((null? tree-tmp) 0);空树
          ((not (pair? tree-tmp));叶子
           (if (odd? line);单数层
               (* line tree-tmp)
               0))
          (else
           (begin (if (pair? (car tree-tmp)) 
           (+ (tree-line (car tree-tmp) (+ line 1))
              (tree-line (cdr tree-tmp) line))
           (+ (tree-line (car tree-tmp) line)
              (tree-line (cdr tree-tmp) line)))))))
  (tree-line tree 1))

(define (loop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin
          (displayln (sum-odd-tree a))
          (loop)))))

(loop)