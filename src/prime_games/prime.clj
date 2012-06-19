(ns prime-games.prime
  (:use [clojure.math.numeric-tower :only (sqrt)]))

(defn prime? [n]
  (defn divisible? [dividend divisor]
    (= 0 (rem dividend divisor)))

  (defn six*k+-1-test [number idx]
    (cond (> (- (* 6 idx) 1) (sqrt number)) true
          (divisible? number (+ (* 6 idx) 1)) false
          (divisible? number (- (* 6 idx) 1)) false
          :else (six*k+-1-test number (+ idx 1))))

  (cond (or (= n 2)
            (= n 3)) true
        (or (even? n)
            (= n 1)) false
            (divisible? n 3) false
            :else (six*k+-1-test n 1)))