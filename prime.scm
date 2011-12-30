(define-module (prime)
  #:export (prime? prime-digits?))

(define (prime? number)

  (define (divisible? number divisor)
    (= 0 (remainder number divisor)))

  (define (even? number)
    (divisible? number 2))

  (define (6k+-1-test number idx)
    (cond ((> (- (* 6 idx) 1) (sqrt number))
           #t)
          ((divisible? number (+ (* 6 idx) 1))
           #f)
          ((divisible? number (- (* 6 idx) 1))
           #f)
          (else
           (6k+-1-test number (+ idx 1)))))

  (cond ((= number 2)
         #t)
        ((or (even? number)
             (= number 1))
         #f)
        ((divisible? number 3)
         #f)
        (else
         (6k+-1-test number 1))))

(define (prime-digits? number)
  (cond ((and (< number 10)
              (prime? number))
         #t)
        ((< number 10) #f)
        (else (and (prime-digits? (modulo number 10))
                   (prime-digits? (quotient number 10))))))
