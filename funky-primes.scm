(define (naive-prime? number)

  (define (divisible? number divisor)
    (= 0 (remainder number divisor)))

  (define (even? number)
    (divisible? number 2))

  (define (naive-test number divisor)
    (cond ((> divisor (sqrt number))
           #t)
          ((divisible? number divisor)
           #f)
          (else
           (if (even? divisor)
               (naive-test number (+ divisor 1))
               (naive-test number (+ divisor 2))))))

  (cond ((= number 2)
         #t)
        ((or (even? number)
             (= number 1))
         #f)
        (else
         (naive-test number 2))))

(define (prime-digits? num)
  (cond ((and (< num 10)
              (naive-prime? num))
         #t)
        ((< num 10) #f)
        (else (and (prime-digits? (modulo num 10))
                   (prime-digits? (quotient num 10))))))

(define (digit-sum num)
  (if (< num 10)
      num
      (+ (modulo num 10)
         (digit-sum (quotient num 10)))))

(define (funky-prime? number)
  (and (naive-prime? number)
       (naive-prime? (digit-sum number))
       (prime-digits? number)))

(use-modules (ice-9 streams))

(define (funky-primes-in-range low high)
  (let ((numbers (make-stream (lambda (state)
                                (if (>= state high)
                                    state
                                    (cons state (+ state 1))))
                              low)))
    (stream-fold (lambda (number previous)
                   (if (funky-prime? number)
                       (cons number previous)
                       previous))
                 '()
                 numbers)))

(define (primes-in-range low high)
  (let ((numbers (make-stream (lambda (state)
                                (if (>= state high)
                                    state
                                    (cons state (+ state 1))))
                              low)))

    (stream-fold (lambda (number previous)
                   (if (naive-prime? number)
                       (cons number previous)
                       previous))
                 '()
                 numbers)))


