(ns prime-games.funky-primes
  (:use [prime-games.prime :only (prime?)]))

(defn digit-sum [number]
  (if (< number 10)
    number
    (+ (rem number 10)
       (digit-sum (quot number 10)))))

(defn prime-digits? [number]
  (if (< number 10)
    (prime? number)
    (and (prime? (rem number 10))
         (prime-digits? (quot number 10)))))

(defn funky-prime? [number]
  (and (prime? number)
       (prime? (digit-sum number))
       (prime-digits? number)))

(defn funky-primes-in-range [low high]
  (let [numbers (range low (inc high))]
    (filter funky-prime? numbers)))


