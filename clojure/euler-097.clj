(defn problem-97 []
  (loop [total 28433 times 7830457]
    (if (zero? times)
      total
      (recur (mod (* total 2) 10000000000) (dec times)))))
  
