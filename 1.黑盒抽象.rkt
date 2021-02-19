#lang racket
(require r5rs)

;;前缀表达式
(+ 1 3 4)

(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))

;;定义函数

;;
(define pi 3.14159)
pi
(define e 2.718)
(define phi 1.618)
(define D-cup 17.5)
e
phi
D-cup

;;
(define square
  (lambda (x)
    (* x x)))

(square 2)

(define sum-of-square
  (lambda (x y)
    (+ (square 3) (square 6)))) 

;;条件判断

(define abs
  (lambda (x)
    (cond ((< x 0) (- x))
          ((> x 0) x)
          ((= x 0) 0))))

;;heron法求平方根

(define sqrt-iter
  (lambda (guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x))))
(define improve
  (lambda (guess x)
    (average guess (/ x guess))))
(define average
  (lambda (x y)
    (/ (+ x y) 2)))
(define good-enough?
  (lambda (guess x)
    (< (abs (- (square guess) x)) 0.001)))
;;(define my-sqrt
;;  (lambda (x)
;;    (sqrt-iter 1.0 x)))
;;(my-sqrt 4)
;;(my-sqrt 3)

;;嵌套定义
(define my-sqrt
  (lambda (x)
    (define improve
      (lambda (guess x)
        (average guess (/ x guess))))
    (define average
     (lambda (x y)
       (/ (+ x y) 2)))
    (define good-enough?
     (lambda (guess x)
       (< (abs (- (square guess) x)) 0.001)))
    (define sqrt-iter
      (lambda (guess x)
        (if (good-enough? guess x)
            guess
        (sqrt-iter (improve guess x) x))))
   (sqrt-iter 1.0 x)))

(my-sqrt 4)
(my-sqrt 3)   


