;;; list and tree, emumerate-->filter-->map-->accumulate

#lang racket
(require r6rs)

;; list

(define l (list 1 2 3 4))

(car l)
(car (cdr l))
(cadr l)
(caddr l)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))
(length odds)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons
       (car list1)
       (append (cdr list1) list2))))

;; map list

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define square
  (λ (x) (* x x)))

(caddr (map square (list 1 2 3 4)))

;; tree

(define t (cons (list 1 2)
                (list 3 4)))
(length t)

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(count-leaves t)

;; map tree
(define (scale-tree tree factor)
  (map (λ (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

(scale-tree t 10)

;; list as interface
(define (filter predicate sequens)
  (cond ((null? sequens) '())
        ((predicate (car sequens))
         (cons (car sequens)
               (filter predicate (cdr sequens))))
        (else (filter predicate (cdr sequens)))))

(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequens)
  (if (null? sequens)
      initial
      (op (car sequens)
          (accumulate op initial (cdr sequens)))))

(accumulate * 1 (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(enumerate-tree t)

(define (sum-odd-square tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(sum-odd-square t)

;; use many times map

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair)
        (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (flatmap proc seq)
  (accumulate append
              '()
              (map proc seq)))

(define (prime? n)
  (let loop ((d 2))
    (cond ((< n (* d d)) #t)
          ((zero? (modulo n d)) #f)
          (else (loop (+ d 1))))))

