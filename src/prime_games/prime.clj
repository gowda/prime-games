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

(defn prime? [n]
  (six*k+-1-prime? n))
