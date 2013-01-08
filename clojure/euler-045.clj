(defn triangle-number [n]
  "Generate the nth triangle number"
  (/ (* n (+ n 1)) 2))

(defn pentagonal-number [n]
  "Generate the nth pentagonal number"
  (/ (* n (- (* 3 n) 1)) 2))

(defn hexagonal-number [n]
  "Generate the nth hexagonal number"
  (* n (- (* n 2) 1)))

(defn find-next-equal-triad [t-orig p-orig h-orig]
  (loop [t t-orig p p-orig h h-orig]
      (let [t-num (triangle-number t)
            p-num (pentagonal-number p)
            h-num (hexagonal-number h)]
        (if (= t-num p-num h-num)
          [t p h]
          (if (= t-num p-num)
            (if (> t-num h-num)
              (recur t p (+ h 1))
              (recur (+ t 1) p h))
            (if (> t-num p-num)
              (recur t (+ p 1) h)
              (recur (+ t 1) p h))
            )))))

(find-next-equal-triad 286 165 141)

(triangle-number 55385)

