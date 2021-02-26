#lang racket
(require r6rs)

;; set (list)
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else
         (intersection-set (cdr set1) set2))))

;; test         
(element-of-set? 2 (list 2 4 6 8))         
            
;; set (sort list)         

(define (element-of-set-2? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else
         (element-of-set-2? x (cdr set)))))
          
(define (intersection-set-2 set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set-2 (cdr set1)
                                         (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;; test
(define S1 (list 2 4 6 8 10))
(define S2 (list 4 6 8 16 18))
(intersection-set-2 S1 S2)

;; set (binary tree)

(define (entry tree) (car tree))
(define (left-branch tree)
  (cadr tree))
(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set-3? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set-3? x (left-branch set)))
        ((> x (entry set))
         (element-of-set-3? x (right-branch set)))))

(define (adjoin-set-2 x set)
  (cond ((null? set)
         (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set-2 x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


;; test
(define my-tree
  (make-tree 5
             (make-tree 1 '() '())
             (make-tree 9 '() '())))

(element-of-set-3? 1 my-tree)