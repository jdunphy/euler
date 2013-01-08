(use 'clojure.set)

(defn factors [num]
  (let [max (int (Math/ceil (Math/sqrt num)))]
    (loop [test-num 2 factors #{1}]
      (if (> test-num max)
        factors
        (if (zero? (mod num test-num))
          (recur (inc test-num) (conj (conj factors test-num) (quot num test-num)))
          (recur (inc test-num) factors))))))

(defn abundant? [num] (< num (apply + (factors num))))


(defn problem-23 []
  (loop [full-set (set (range 1 28124))
         abundants (filter #(abundant? %) (range 10 28123))
         current-abundant 12]
    (if (= abundants ())
      full-set
      (recur (difference full-set (set (map #(+ current-abundant %) abundants)))
             (rest abundants)
             (first (rest abundants))))))


