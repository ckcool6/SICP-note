#lang racket
(require r5rs)

;; code

;; rules of rat-numbers compute
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
         
;; create rat-numbers

(define gcd
  (lambda (a b)
    (if (= b 0)
        a
        (gcd b (remainder a b)))))

(define (make-rat N D)
  (let ((g (gcd N D)))
    (cons (/ N g) (/ D g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; call 
(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

;; create "cons" "car" "cdr"

(define (my-cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "argument not 0 or 1 --cons" m))))
  dispatch)

(define (my-car z) (z 0))
(define (my-cdr z) (z 1))

;; create numbers

(define zero (lambda (f)
               (lambda (x) x)))

zero

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

add-1

;; test
(my-car (my-cons 2 3))
(my-cdr (my-cons 8 7))
(my-car (my-cons 4 5))

