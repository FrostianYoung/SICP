#lang racket
;code begin
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? exp)
  (symbol? exp))

(define (same-variable? exp num)
  (and (variable? exp) (variable? num) (eq? exp num)))

(define (make-sum a1 a2)
  (define (m-list x)
    (if (pair? x)
        x
        (list x)))
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (append (m-list a1) '(+) (m-list a2)))))

(define (make-product a1 a2)
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2) (* a1 a2)))
        (else
         (list a1 '* a2))))

(define (sum? x)
  (define (sum?-iter exp)
    (if (null? exp)
        #f
        (if (eq? (car exp) '+)
            #t
            (sum?-iter (cdr exp)))))
  (and (pair? x) (sum?-iter x)))

(define (addend x);被加数
  (define (addend-iter a b)
    (if (eq? (car b) '+)
        (if (null? (cdr a))
            (car a)
            a)
        (addend-iter (append a (list (car b))) (cdr b))))
  (addend-iter '() x))

(define (augend x);加数
  (define (augend-iter b)
    (if (eq? (car b) '+)
        (let ((tmp (cdr b)))
          (if (null? (cdr tmp))
              (car tmp)
              tmp))
        (augend-iter (cdr b))))
  (augend-iter x))

(define (product? x);乘式
  (cond ((not (pair? x)) #f)
        ((null? (cdr x)) #f)
        ((eq? (cadr x) '*) #t)
        (else #f)))

(define (multiplier p)
  (car p))

(define (multiplicand p)
  (let ((tmp (cddr p)))
    (if (null? (cdr tmp))
        (car tmp)
        tmp)))
;code end



    (define (deriv exp var)
      (cond ((number? exp ) 0)
            ((variable? exp)
             (if (same-variable? exp var) 1 0))
            ((sum? exp)
             (make-sum (deriv (addend exp) var)
                       (deriv (augend exp) var)))
            ((product? exp)
             (make-sum 
              (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
              (make-product (deriv (multiplier exp) var)
                            (multiplicand exp))))
            (else 
             (error "wrong format"))))

    (define (myloop)
      (let ((a (read)))
        (if (eq? a eof)
            (void)
            (begin (display (deriv a 'x)) (newline) (myloop)))))

    (myloop)