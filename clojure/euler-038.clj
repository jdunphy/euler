(defn pandigital-9? [n]
  (= "123456789" (apply str (sort (rest (.split (str n) ""))))))


(defn largest-pan [a-seq]
  (loop [i 1 the-max 1]
    (let [prod (bigint (apply str (map #(* i %) a-seq)))]
      (cond
       (> prod 987654321) the-max
       (and (pandigital-9? prod) (> prod the-max)) (recur (inc i) prod)
       true (recur (inc i) the-max)
       ))))
