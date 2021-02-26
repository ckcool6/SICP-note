;;#lang racket
;;(require r5rs)

;; code
;; sum
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;;cube sum

(define inc
  (lambda (n)
    (+ n 1)))
(define cube
  (lambda (x)
    (* x x x)))
(define sum-cubes
  (lambda (a b)
    (sum cube a inc b)))

(sum-cubes 1 3)

;;int sum

(define id
  (lambda (x) x))
(define (sum-int a b)
  (sum id a inc b))

(sum-int 1 100)

;; 
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))

;;intergral

(define (intergral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(intergral cube 0 1 0.01)

;;use "let"

(define square
  (lambda (x) (* x x)))
(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f-2 x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;;fixed-point search
(define tolerance 0.00001)

(define (fixed-point function first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (function guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;;fixed-point of cos
(fixed-point cos 1.0)

;; y = siny + cosy
(fixed-point (lambda (y)
               (+ (sin y) (cos y)))
             1.0)

;;improve "square root" with fixed-point method
(define average
  (lambda (x y)
    (/ (+ x y) 2)))

(define (sqrt x)
  (fixed-point (lambda (y)
                 (average y (/ x y)))
               1.0))
(sqrt 4)
                 
;;average-damp tenique

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define square-2
  (lambda (x)
    (* x x)))

((average-damp square-2) 10)

;;use average-damp

(define (sqrt-2 x)
  (fixed-point (average-damp
                (lambda (y)
                  (/ x y)))
               1.0))

(sqrt-2 9)

;; cube root

(define (cube-root x)
  (fixed-point (average-damp
                (lambda (y)
                  (/ x (square y))))
               1.0))

(cube-root 3)

;;newton method

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (cube-2 x)
  (* x x x))

((deriv cube-2) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-3 x)
  (newton-method (lambda (y)
                   (- (square y) x))
                 1.0))

(sqrt-3 4)
