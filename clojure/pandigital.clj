(ns pandigital
  (:require [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as combo]))

(def digits (set "123456789"))

(defn pandigital? [numbers]
  "Returns true if input numbers are 1-9 pandigital. Input: list of numbers"
  (let [num-str (apply str numbers)]
    (and
     (= 9 (.length num-str))
     (= (set (apply str numbers)) digits))))

(defn largest-pandigital-prime [start]
  (loop [num start]
    (println num)
    (if (and (pandigital-num? num) (prime? num) )
      num
      (recur (- num 2)))))

(defn pandigital-with-product? [a b] (pandigital? [a b (* a b)]))

(defn vectors [i] (map #(vector i %) (range i 9000)))

;; problem 32 solution
(defn time-test []
  (time (apply + (set (map #(apply * %)  (filter #(apply pandigital-with-product? %) (mapcat #(vectors %) (range 100))))))))

(defn pandigital-num? [number]
  "Returns true if number is pandigital. Works for all numbers < 10 digits long."
  (let [to-digits (set (str number)) num-digits (count (str number))]
    (and (= (count to-digits) num-digits)
         (= to-digits (set (take num-digits "123456789"))))))

(defn prime? [n]
  (let [divisors (concat [2 3] (range 5 (inc (math/ceil (math/sqrt n))) 2))]
    (not-any? #(zero? (mod n %)) divisors)))

;; problem 41 solution
;; largest n-digit pandigital prime
(def largest-pandigital
  (last (sort (filter prime?
                      (map #(Integer/parseInt (apply str %)) (combo/permutations (reverse (range 1 8))))))))


;; problem 43 is also a pandigital problem 0-9 pandigital
