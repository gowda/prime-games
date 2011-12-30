(define (msd number)
  (if (= (quotient number 10)
         0)
      number
      (msd (quotient number 10))))

(define (lsd number)
  (modulo number 10))

(define (strip-lsd number)
  (quotient number 10))

(define (strip-msd number)
  (if (= (msd number)
         number)
      0
      (+ (lsd number) (* 10 (strip-msd (strip-lsd number))))))

(define (strip-ends number)
  (strip-msd (strip-lsd number)))

(define (palindrome? number)
  (cond ((< number 10) #t)
        ((= (msd number)
            (lsd number))
         (palindrome? (strip-ends number)))
        (else #f)))

(use-modules (ice-9 streams))
(use-modules (prime))

(define (palindromes-primes-in-range low high)
  (let ((numbers (make-stream (lambda (state)
                                (if (>= state high)
                                    state
                                    (cons state (+ state 1))))
                              low)))

    (stream-fold (lambda (number previous)
                   (if (and (prime? number)
                            (palindrome? number))
                       (cons number previous)
                       previous))
                 '()
                 numbers)))
