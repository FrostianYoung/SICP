#lang racket
(require r5rs)


(define (fron queue) (car queue))
(define (after queue) (cdr queue))
(define (set-front! queue x)
  (set-car! queue x))
(define (set-after! queue x)
  (set-cdr! queue x))
(define (empty-q? queue)
  (null? (fron queue)))
(define (make-q) (cons '() '()))
(define (front-q queue)
  (if (empty-q? queue)
      (error "front-q an empty queue" queue)
      (car (fron queue))))
(define (insert-q! queue x)
  (let ((y (cons x '())))
    (cond ((empty-q? queue)
           (set-front! queue y)
           (set-after! queue y)
           queue)
          (else
           (set-cdr! (after queue) y)
           (set-after! queue y)
           queue))))
(define (delete-q! queue)
  (cond ((empty-q? queue)
         (error "delete an empty queue" queue))
        (else (set-front! queue (cdr (fron queue)))
              queue)))


(define (set-map);地图
  (list '*map*))

(define (find x y map)
  (let ((a (assoc x (cdr map))))
    (if a
        (let ((b (assoc y (cdr a))))
          (if b
              (cdr b)
              #f))
        #f)))

(define (insert! x y val map)
  (let ((a (assoc x (cdr map))))
    (if a
        (let ((b (assoc y (cdr a))))
          (if b
              (set-cdr! b val)
              (set-cdr! a (cons (cons y val) (cdr a)))))
        (set-cdr! map (cons (list x (cons y val)) (cdr map)))))
  'ok)

(define visited '())

(define (read-map r c map)
  (define (loop1 r c map thisr)
  (if (= r thisr)
      map
      (begin (loop2 r c map thisr 0)
             (loop1 r c map (+ thisr 1)))))
  (loop1 r c map 0))

(define (loop2 r c map thisr thisc)
  (if (= c thisc)
      map
      (let ((l (read)))
        (insert! thisr thisc l map)
        (loop2 r c map thisr (+ thisc 1)))))

(define (search map qu r c)
  (if (empty-q? qu)
      (displayln "inf")
      (let ((fr (front-q qu)))
        (let ((thisr (car fr))
              (thisc (cadr fr))
              (thisk (caddr fr))
              (step (cadddr fr)))
          (set! visited (cons (list thisr thisc) visited))
          (delete-q! qu)
          (if (and (= thisr r) (= thisc c))
              (displayln step)
              (begin (try-insert qu map (- thisr 1) thisc thisk (+ step 1) r c)
                     (try-insert qu map (+ thisr 1) thisc thisk (+ step 1) r c)
                     (try-insert qu map thisr (- thisc 1) thisk (+ step 1) r c)
                     (try-insert qu map thisr (+ thisc 1) thisk (+ step 1) r c)
                     (search map qu r c)))))))

(define (try-insert qu map thisr thisc thisk step r c)
  (if (or (< thisr 0) (< thisc 0) (> thisr r) (> thisc c))
      (void)
      (let ((l (find thisr thisc map)))
        (if (member (list thisr thisc) visited)
            (void)
            (cond ((eq? l 'W) (void))
                  ((eq? l 'M)
                   (if (< (- thisk 1) 0)
                       (void)
                       (insert-q! qu (list thisr thisc (- thisk 1) step))))
                  (else (insert-q! qu (list thisr thisc thisk step))))))))

(define (myloop n)
  (if (= n 0)
      (void)
      (begin
        (set! visited '())
        (let ((r (read)) (c (read)) (k (read)))
          (let ((map (read-map r c (set-map)))
                (qu (insert-q! (make-q) (list 0 0 k 0))))
            (search map qu (- r 1) (- c 1))))
        (myloop (- n 1)))))

(myloop (read))