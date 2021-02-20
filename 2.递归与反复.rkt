#lang racket
(require r5rs)

;; code
;; factorial example, recursive version
(define factorial
  (lambda (n)
    (if (= n 1)
        1
        (* n (factorial (- n 1))))))

(factorial 8)

;; tail call version

(define factorial-iter
  (lambda (product counter max-count)
    (if (> counter max-count)
	product
	(factorial-iter (* counter product)
			(+ counter 1)
			max-count))))
(define factorial-2
  (lambda (n)
    (factorial-iter 1 1 n)))

(factorial-2 6)

;; fibnacci

(define fibnacci
  (lambda (n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fibnacci (- n 1))
                   (fibnacci (- n 2)))))))

(fibnacci 6)

;; iteralble version
(define fibnacci-iter
  (lambda (a b counter)
    (if (= counter 0)
        b
        (fibnacci-iter (+ a b) a (- counter 1)))))

(define fibnacci-2
  (lambda (n)
    (fibnacci-iter 1 0 n)))

(fibnacci-2 6)


;;
(define expt
  (lambda (b n)
    (if (= n 0)
        1
        (* b (expt b (- n 1))))))

(expt 2 5)

;; iteralble version
(define expt-iter
  (lambda (b counter product)
    (if (= counter 0)
        product
        (expt-iter b
                   (- counter 1)
                   (* b product)))))

(define expt-2
  (lambda (b n)
    (expt-iter b n 1)))

(expt-2 2 5)

;; GCD

(define gcd
  (lambda (a b)
    (if (= b 0)
        a
        (gcd b (remainder a b)))))

(gcd 206 40)

;;
(define output-iter
  (lambda (i n)
    (if (<= i n)
        (begin
          (display "hello,world!")
          (newline)
          (output-iter (+ i 1) n)))))
(define output-n
  (lambda (n)
    (output-iter 1 n)))
(output-n 6)


