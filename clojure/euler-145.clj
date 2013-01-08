(use 'clojure.set)

(defn reverse-num [n]
  (bigint (apply str (reverse (str n)))))

(def odds #{"1" "3" "5" "7" "9"})

(defn odd-digits? [n]
  (= #{} (difference (set (rest (.split (str n) ""))) odds)))

(defn reversible? [n]
  (let [rev (reverse-num n)]
    (and
     (odd-digits? (+ n rev))
     (not (= 0 (mod n 10))))))


(defn quick-fail [n]
  
  )

(defn f-rev? [n]
  (if (= 0 (mod n 10))
    false
    (odd-digits? (+ n (reverse-num n)))))
