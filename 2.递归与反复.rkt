#lang racket
(require r5rs)

;; code
;; 阶乘例子
(define factorial
  (λ (n)
    (if (= n 1)
        1
        (* n (factorial (- n 1))))))

(factorial 8)
