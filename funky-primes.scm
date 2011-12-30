(use-modules (prime))

(define (digit-sum num)
  (if (< num 10)
      num
      (+ (modulo num 10)
         (digit-sum (quotient num 10)))))

(define (funky-prime? number)
  (and (prime? number)
       (prime? (digit-sum number))
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
                   (if (prime? number)
                       (cons number previous)
                       previous))
                 '()
                 numbers)))


