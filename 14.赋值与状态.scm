#lang racket
(require r6rs)

;;code
(define balance 100)
(define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))

;;test
balance
(withdraw 20)
(withdraw 50)
(withdraw 40)

;;
(define new-withdraw
    (let ((balance 100))
      (lambda (amount)
        (if (>= balance amount)
            (begin
              (set! balance (- balance amount))
              balance)
            "Insufficient funds"))))

;;test
(new-withdraw 40)
(new-withdraw 70)

;;
(define (make-withdraw balance)
    (lambda (amount)
      (if (>= balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          "Insufficient funds")))
(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

;;test
(W1 50)
(W2 50)

(define (make-account balance)
    (define (withdraw amount)
      (if (>= balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
        ((eq? m 'deposit) deposit)
        (else
          (error "Unknow request -- make-account" m))))
    dispatch)

;;test
(define acc (make-account 100))
((acc 'withdraw) 60)
((acc 'deposit) 40)
(define acc2 (make-account 110))
((acc 'withdraw) 60)
((acc2 'withdraw) 40)

;;
(define (make-decrementer balance)
    (lambda (amount)
      (- balance amount)))

(define D (make-decrementer 25))
;;test
(D 20)
(D 10)

;;
(random 5)
(random 5)
(random 5)
