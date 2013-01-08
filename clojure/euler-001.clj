(defn fizzy [n]
  (or
   (== 0 (mod n 3))
   (== 0 (mod n 5))))

(defn sum-fb [n]
  (if (== n 0)
    0
    (+ (if (fizzy n) n 0)
       (sum-fb (dec n)))))

(println (sum-fb 999))

;; solution 2
;; blergh
(apply + (filter fizzy (range 1 1000)))
