#lang racket
(define (square x) (mul x x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;以下是put和get的实现,不须搞明白也能完成本题
(require scheme/mpair)

(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                            (mcons (mcons key-2 value)
                                  (mcdr subtable)))))
            (set-mcdr! local-table
                      (mcons (mlist key-1
                                  (mcons key-2 value))
                            (mcdr local-table)))))
      (void))    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define conversion-table (make-table))
(define get-coercion (conversion-table 'lookup-proc))
(define put-coercion (conversion-table 'insert-proc!))
;以上是put和get的实现,不须搞明白也能完成本题
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))


(define (install-complex-package)
  (define (real-part z) (cadr z))
  (define (imag-part z) (caddr z))
  (define (make x y) (list 'complex x y))
  (define (neg z) (make
                   (apply-generic 'neg (real-part z))
                   (apply-generic 'neg (imag-part z))))
  (define (add-complex z1 z2)
    (make (add (real-part z1) (real-part z2))
          (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make (sub (real-part z1) (real-part z2))
          (sub (imag-part z1) (imag-part z2))))
 (define (mul-complex z1 z2)
   (let ((a (real-part z1))
         (b (imag-part z1))
         (c (real-part z2))
         (d (imag-part z2)))
     (make (sub (mul a c) (mul b d)) (add (mul a d) (mul b c)))))
 (define (div-complex z1 z2)
   (let ((a (real-part z1))
         (b (imag-part z1))
         (c (real-part z2))
         (d (imag-part z2)))
     (let ((denom (add (square c) (square d))))
       (make (div (add (mul a c) (mul b d)) denom)
             (div (sub (mul b c) (mul a d)) denom)))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'make 'complex make)
  (put 'neg '(complex) neg)
  (put 'add '(complex complex) add-complex)
  (put 'sub '(complex complex) sub-complex)
  (put 'mul '(complex complex) mul-complex)
  (put 'div '(complex complex) div-complex)
  (void))


(define (install-rational-package)
  (define (numer x) (cadr x))
  (define (denom x) (caddr x))
  (define (make n d) (list 'rational n d))
  (define (neg x) (make (apply-generic 'neg (numer x)) (denom x)))
  (define (add-rat x y)
    (make (add (mul (numer x) (denom y))
               (mul (numer y) (denom x)))
          (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make (sub (mul (numer x) (denom y))
               (mul (numer y) (denom x)))
          (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make (mul (numer x) (numer y))
          (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make (mul (numer x) (denom y))
          (mul (denom x) (numer y))))
  
  (put 'add '(rational rational) add-rat)
  (put 'sub '(rational rational) sub-rat)
  (put 'mul '(rational rational) mul-rat)
  (put 'div '(rational rational) div-rat)
  (put 'neg '(rational) neg)
  (put 'rational 'numer numer)
  (put 'rational 'denom denom)
  (put 'make 'rational make)
  (void))



(define (install-integer-package)
  (define (make n) (list 'integer n))
  (define (int i) (cadr i))
  (define (neg i) (make (- (int i))))
  (put 'add '(integer integer)
       (lambda (x y) (make (+ (cadr x) (cadr y)))))
  (put 'sub '(integer integer)
       (lambda (x y) (make (- (cadr x) (cadr y)))))
  (put 'mul '(integer integer)
       (lambda (x y) (make (* (cadr x) (cadr y)))))
  (put 'div '(integer integer) (get 'make 'rational))
  (put 'make 'integer make)
  (put 'int 'integer int)
  (put 'neg '(integer) neg)
  (void))


(define (make-complex a b) ((get 'make 'complex) a b))
(define (make-integer n) ((get 'make 'integer) n))
(define (make-rational n d) ((get 'make 'rational) n d))
(define (is-rational? v) (eq? (type-tag v) 'rational))
(define (is-integer? v) (eq? (type-tag v) 'integer))
(define (is-complex? v) (eq? (type-tag v) 'complex))
(define (int i) ((get 'int 'integer) i))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (numer r) ((get 'rational 'numer) r))
(define (denom r) ((get 'rational 'denom) r))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (neg a) (apply-generic 'neg a))


(define (install-raise-package)
  (define (raise-integer n)
    (make-rational n (make-integer 1)))
  (define (raise-rational x)
    (make-complex x (make-integer 0)))
  (put 'raise 'integer raise-integer)
  (put 'raise 'rational raise-rational)
  (void))
(define (raisable v1 v2)
  (cond ((eq? (type-tag v1) (type-tag v2)) (list #t v1))
        ((eq? (type-tag v1) 'complex) (list #f))
        (else (raisable ((get 'raise (type-tag v1)) v1) v2))))
(define (apply-generic op . args)
  (let ((proc (get op (map type-tag args))))
    (if proc
        (apply proc args)
        (if (= (length args) 2)
            (let ((v1 (car args))
                  (v2 (cadr args)))
              (let ((res (raisable v1 v2)))
                (if (car res)
                    ((get op (list (type-tag v2) (type-tag v2))) (cadr res) v2)
                    ((get op (list (type-tag v1) (type-tag v1))) v1 (cadr (raisable v2 v1))))))
            (error "Can't perform type promotion with != 2 args")))))

(install-rational-package)
(install-integer-package)
(install-complex-package)
(install-raise-package)

(define (simplify a)
  (cond
((is-integer? a) a)
((is-rational? a)
 (cond ((equal? (numer a) (denom a))
        (make-integer 1))
       ((and (not (equal? (numer a) (simplify (numer a))))
             (not (equal? (denom a) (simplify (denom a)))))
        (make-rational
         (simplify (numer a))
         (simplify (denom a))))
       ((and (is-integer? (denom a))
             (= (int (denom a)) 1))
        (simplify (numer a)))
       ((and (is-integer? (numer a))
             (= (int (numer a)) 0))
        (make-integer 0))
       ((and (is-integer? (denom a))
             (< (int (denom a)) 0))
        (simplify
         (make-rational
          (neg (numer a)) (neg (denom a)))))
       ((and (is-integer? (numer a))
             (is-integer? (denom a)))
        (let ((g (gcd (int (numer a))
                      (int (denom a)))))
          (make-rational
           (make-integer (/ (int (numer a)) g))
           (make-integer (/ (int (denom a)) g)))))
       ((is-rational? (numer a))
        (simplify
         (make-rational
          (numer (numer a))
          (simplify (mul (denom (numer a)) (denom a))))))
       ((is-rational? (denom a))
        (simplify
         (make-rational
          (simplify (mul (numer a) (denom (denom a))))
          (numer (denom a)))))
       ((is-complex? (numer a))
        (simplify
         (make-complex
          (make-rational (real-part (numer a)) (denom a))
          (make-rational (imag-part (numer a)) (denom a)))))
       ((is-complex? (denom a))
        (let ((dn (simplify (add
                    (mul (real-part (denom a)) (real-part (denom a)))
                    (mul (imag-part (denom a)) (imag-part (denom a)))))))
          (simplify
           (make-complex
            (make-rational
             (simplify (mul (numer a) (real-part (denom a))))
             dn)
            (neg (make-rational
                  (simplify (mul (numer a) (imag-part (denom a))))
                  dn))))))
       (else a)))
((is-complex? a)
 (cond ((and (is-integer? (imag-part a))
             (= (int (imag-part a)) 0))
        (simplify (real-part a)))
       ((and (is-rational? (real-part a))
             (not (equal? (real-part a) (simplify (real-part a)))))
        (simplify
         (make-complex
          (simplify (real-part a))
          (imag-part a))))
       ((and (is-rational? (imag-part a))
             (not (equal? (imag-part a) (simplify (imag-part a)))))
        (simplify
         (make-complex
          (real-part a)
          (simplify (imag-part a)))))
       ((is-complex? (real-part a))
        (simplify
         (make-complex
          (real-part (real-part a))
          (add (imag-part (real-part a))
               (imag-part a)))))
       ((is-complex? (imag-part a))
        (simplify
         (make-complex
          (sub (real-part a) (imag-part (imag-part a)))
          (real-part (imag-part a)))))
       (else a)))))

(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (displayln (simplify (simplify a)))
               (myloop)))))

(myloop)
