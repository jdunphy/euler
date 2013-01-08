(defn pal? [n] (= (str n) (apply str (reverse (str n)))))

(defn reverse-num [n] (bigint (apply str (reverse (str n)))))

(defn lychrel? [num]
  (loop [i 1 n (+ num (reverse-num num))]
    (if (= i 50)
      true
      (if (pal? n)
        false
        (recur (inc i) (bigint (+ n (reverse-num n))))))))
