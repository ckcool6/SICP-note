
;; one-d table

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
	(cdr record)
	#f)))

(define (assoc key records)
  (cond ((null? records)
	 #f)
	((equal? key (caar records))
	 (car records))
	(else
	 (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table
		  (cons
		   (cons key value)
		   (cdr table)))))
  'insert-OK)

(define (make-table)
  (list '*table*))

;; test

(define sheet (make-table))

(insert! 'a 1 sheet)
(insert! 'b 2 sheet)
(insert! 'c 3 sheet)
(insert! 'd 4 sheet)

sheet

(lookup 'd sheet)
(lookup 'a sheet)

;; two-d table

(define (lookup-2 key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	 (let ((record (assoc key-2 (cdr subtable))))
	   (if record
	       (cdr record)
	       #f))
	 #f)))

(define (insert-! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record
	      (set-cdr! record value)
	      (set-cdr! subtable
			(cons (cons key-2 value)
			      (cdr subtable)))))
	(set-cdr! table
		  (cons (list key-1
			      (cons key-2 value))
			(cdr table)))))
  'OK)

;; create local table

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup-2 key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  #f))
	    #f)))
    (define (insert-! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1
				  (cons key-2 value))
			    (cdr local-table)))))
      'OK)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup-2)
	    ((eq? m 'insert-proc!) insert-!)
	    (else
	     (error "Unknow operation -- TABLE" m))))
    dispatch))

;; put and get

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;;test
(put 'math '+ 4)
(put 'math '- 7)
(get 'math '+)
