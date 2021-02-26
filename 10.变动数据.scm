;; set! & set-car! & set-cdr!, queue structure

;;; test 

(define x
  (list
   (list 'a 'b) 'c 'd))
x
(define y
  (list 'e 'f))
y

(set-car! x y)
x
(define long 15)
u
(set! long 10)
u

(define z
  (cons y (cdr x)))
z

(set-cdr! x y)
x

;; test2 share
(define x (list 'a 'b))
(define z1 (cons x x))
z1

(define z2 (cons (list 'a 'b)
		 (list 'a 'b)))
z2  ;;z1 z2 same,but structure isn't same

(define (set-to-wow! x)
  (set-car! (car x) 'wow) x)

(set-to-wow! z1)
(set-to-wow! z2) ;; z1, z2 different output

;; check same or not 
(eq? (car z1) (cdr z1))
(eq? (car z2) (cdr z2))

;; remake cons car cdr
(define (cons x y)
  (define (set-x! v)
    (set! x v))
  (define (set-y! v)
    (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
	  ((eq? m 'cdr) y)
	  ((eq? m 'set-car!) set-x!)
	  ((eq? m 'set-cdr!) set-y!)
	  (else
	   (error "Undefined operation -- CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

;; queue structure

;; (make-queue)
;; (empty-queue? <queue>)
;; (front-queue <queue>)
;; (insert-queue! <queue> <item>)
;; (delete-queue! <queue>)

;; code

(define front-pointer
  (lambda (queue)
    (car queue)))

(define rear-pointer
  (lambda (queue)
    (cdr queue)))

(define set-front-ptr!
  (lambda (queue item)
    (set-car! queue item)))

(define set-rear-ptr!
  (lambda (queue item)
    (set-cdr! queue item)))

(define empty-queue?
  (lambda (queue)
    (null? (front-pointer queue))))

(define make-queue
  (lambda () (cons '() '())))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-pointer queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
	   (set-front-ptr! queue new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue)
	  (else
	   (set-cdr! (rear-pointer queue) new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
	 (error "DELETE called with an empty queue" queue))
	(else
	 (set-front-ptr! queue (cdr
				(front-pointer queue)))
	 queue)))

;; test

(define q1 (make-queue))
(insert-queue! q1 'a)
(delete-queue! q1)
