(defn is-palindrome [s] (= s (apply str (reverse s))))

(defn to-binary [i] (Integer/toBinaryString i)) 

(defn huh [x] (and 
               (is-palindrome (str x))
               (is-palindrome (to-binary x))))

; (apply + (filter huh (range 1000000)))
(filter huh (range 1000000))
