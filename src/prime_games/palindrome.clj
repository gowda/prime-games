(ns prime-games.palindrome
  (:use [prime-games.prime :only (prime?)]))

(defn msd [number]
  (if (= (quot number 10)
         0)
    number
    (msd (quot number 10))))

(defn lsd [number]
  (rem number 10))

(defn strip-lsd [number]
  (quot number 10))

(defn strip-msd [number]
  (if (= (msd number)
         number)
    0
    (+ (lsd number) (* 10 (strip-msd (strip-lsd number))))))

(defn strip-ends [number]
  (strip-msd (strip-lsd number)))

(defn palindrome? [number]
  (cond (< number 10) true
        (= (msd number)
           (lsd number)) (palindrome? (strip-ends number))
           :else false))

(defn palindromes-primes-in-range [low high]
  (let [numbers (range low (inc high))]
    (filter #(and (prime? %) (palindrome? %)) numbers)))
