;;use mit-scheme
(define (integers-starting-from n)
  (cons-stream n
               (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y)
  (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x)
                   (not (divisible? x 7)))
                 integers))

(stream-ref no-sevens 100)

;;fibnacci stream
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

;;prime filter
(define (prime? n)
  (let loop ((d 2))
    (cond ((< n (* d d)) #t)
          ((zero? (modulo n d)) #f)
          (else (loop (+ d 1))))))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(stream-ref primes 50)

;;use recursion to define stream
(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1
                              (add-streams ones integers)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-stream (stream-cdr fibs)
                                        fibs))))
(stream-ref fibs 6)

(define (scale-stream stream factor)
  (stream-map (lambda (x)
                (* x factor))
              stream))

(define double (cons-stream 1 (scale-stream double 2)))

(stream-ref double 3)

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(stream-ref primes 8)

;;application of stream
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define average
  (lambda (x y)
    (/ (+ x y) 2)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
        (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(display-stream (sqrt-stream 2))


