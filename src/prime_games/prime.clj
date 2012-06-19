(ns prime-games.prime
  (:use [clojure.math.numeric-tower :only (sqrt)]))

(defn- divisible? [dividend divisor]
  (= 0 (rem dividend divisor)))

;; from http://en.wikipedia.org/wiki/Primality_test
;;
;; This is because all integers can be expressed as (6k + i) for some integer
;; k and for i = −1, 0, 1, 2, 3, or 4; 2 divides (6k + 0), (6k + 2), (6k + 4);
;; and 3 divides (6k + 3). So a more efficient method is to test if n is
;; divisible by 2 or 3, then to check through all the numbers of form
;; 6k ± 1 < sqrt (n).
(defn- six*k+-1-test [number idx]
  (cond (> (- (* 6 idx) 1) (sqrt number)) true
        (divisible? number (+ (* 6 idx) 1)) false
        (divisible? number (- (* 6 idx) 1)) false
        :else (six*k+-1-test number (+ idx 1))))

(defn- six*k+-1-prime? [number]
  (cond (or (= number 2)
            (= number 3)) true
        (or (even? number)
            (= number 1)) false
            (divisible? number 3) false
            :else (six*k+-1-test number 1)))

;; from http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
;;
;; To find all the prime numbers less than or equal to a given integer n by
;; Eratosthenes' method:
;; 1. Create a list of consecutive integers from 2 to n: (2, 3, 4, ..., n).
;; 2. Initially, let p equal 2, the first prime number.
;; 3. Starting from p, count up in increments of p and mark each of these
;;    numbers greater than p itself in the list. These numbers will be 2p, 3p,
;;    4p, etc.; note that some of them may have already been marked.
;; 4. Find the first number greater than p in the list that is not marked. If
;;    there was no such number, stop. Otherwise, let p now equal this number
;;    (which is the next prime), and repeat from step 3.
;;
;; When the algorithm terminates, all the numbers in the list that are not
;; marked are prime.
(defn filter-factors [coll primes]
  (drop-while #(some (fn [divisor] (divisible? % divisor)) primes) coll))

(defn- sieve-of-eratosthenes [limit]
  (loop [primes []
         coll (range 2 (inc limit))
         base (first coll)]
    (if (empty? coll)
      primes
      (recur (conj primes base)
             (filter-factors coll (conj primes base))
             (first (filter-factors coll (conj primes base)))))))

(defn sieve-prime? [number]
  (some #{number} (sieve-of-eratosthenes number)))

(defn prime? [n]
  (six*k+-1-prime? n))
