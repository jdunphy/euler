(require 'clojure.set)

(defn fib [n]
  (cond
   (= n 1) [1]
   (= n 2) [1 1]
   true (loop [s [1 1] i2 1 i1 1 max-n (- n 2)]
          (if (zero? max-n)
            s
            (recur (conj s (+ i2 i1)) i1 (+ i2 i1) (dec max-n))))))

(defn my-flatten [s]
  (loop [flat '() orig s]
    (if (empty? orig)
      flat
      (recur (concat flat (first orig)) (rest orig)))))

(defn good-flatten
  [x]
  (filter #(not (sequential? %))
          (tree-seq sequential? seq x)))

(defn dedupe [x]
  (loop [s x new-s '() current nil]
    (if (empty? s)
      (reverse new-s)
      (if (= current (first s))
        (recur (rest s) new-s current)
        (recur (rest s) (conj new-s (first s)) (first s))))))

(defn pack-seq [x]
  (loop [packed '() unpacked x current '()]
    (if (empty? unpacked)
      (rest (reverse (conj packed current)))
      (if (= (first current) (first unpacked))
        (recur packed (rest unpacked) (conj current (first unpacked)))
        (recur (conj packed current) (rest unpacked) (conj '() (first unpacked)))))))

(defn dupe-seq [x]
  (map #(% %) x))

(defn dupe-seq [x]
  (mapcat #(seq [% %]) x))

                                        ; problem 33
(defn dupe-seq-n [the-seq num]
  (mapcat
   (fn [x]
     (loop [s '() n num]
       (if (zero? n)
         s
         (recur (conj s x) (dec n)))) ) the-seq))


(defn interl [x y]
  (loop [left x right y product []]
    (if (or (empty? left) (empty? right))
      product
      (recur (rest left) (rest right) (concat product [(first left) (first right)])))))

(defn interp [in s]
  (loop [new-s [(first s)] orig (rest s)]
    (if (empty? orig)
      new-s
      (recur (concat new-s [in (first orig)]) (rest orig)))))

(defn drop-nth [n s]
  (let [taken #(take (dec n) %)]
    (loop [new-s [] orig s]
      (if (empty? orig)
        new-s
        (recur (concat new-s (taken orig)) (drop n orig))))))

(defn fact [n] (reduce * (range 1 (inc n))))

(defn part-seq [n seq]
  (loop [part-seqs '() rmd-seq seq i 0]
    (if (= n i)
      (reverse part-seqs)
      (recur (conj part-seqs (take-nth n rmd-seq)) (rest rmd-seq) (inc i)))))


(defn rot-seq [num seq]
  (let [n (mod num (count seq))]
    (println n)
    (concat (drop n seq) (take n seq))))


(defn split [n s]
  (cons (take n s) (cons (drop n s) '())))

(defn split-on-type [s]
  (loop [types {} orig s]
    (if (empty? orig)
      (vals types)
      (let [t (type (first orig))]
        (if (contains? types t)
          (recur (assoc types t (conj (get types t) (first orig))) (rest orig))
          (recur (assoc types t [(first orig)]) (rest orig))
          )))))

(defn longest-subseq [s]
  (loop [max-seq [(first s)] current-seq [(first s)] i 1]
    (if (= i (count s))
      (if (< 1 (count max-seq))
        max-seq
        [])
      (let [num (nth s i)]
        (if (= (inc (last current-seq)) num)
          (if (= (count max-seq) (count current-seq))
            (recur (conj current-seq num) (conj current-seq num) (inc i))
            (recur max-seq (conj current-seq num) (inc i)))
          (recur max-seq [num] (inc i)))))))

(defn jd-part [n s]
  (loop [chunks [] rest-s s]
    (if (> n (count rest-s))
      chunks
      (recur (conj chunks (take n rest-s)) (drop n rest-s)))))

(defn count-occs [s]
  (loop [occs {} orig s]
    (if (empty? orig)
      occs
      (let [num (first orig)]
        (if (contains? occs num)
          (recur (assoc occs num (inc (get occs num))) (rest orig))
          (recur (assoc occs num 1) (rest orig)))))))

(defn unique [s]
  (loop [found #{} new []  orig s]
    (if (empty? orig)
      new
      (if (contains? found (first orig))
        (recur found new (rest orig))
        (recur (conj found (first orig)) (conj new (first orig)) (rest orig))))))

(defn jd-comp [& funs]
  (fn
     ([arg]
            (loop [res arg to-call (reverse funs)]
              (if (empty? to-call)
                res
                (recur ((first to-call) res ) (rest to-call)))))
    ([arg1 arg2 & args]
       (loop [res (apply (first (reverse funs)) (list* arg1 arg2 args)) to-call (rest (reverse funs)) ]
         (if (empty? to-call)
           res
           (recur ((first to-call) res) (rest to-call)))))))


(defn jd-jux [& funs]
  (fn
    ([arg]
       (loop [res [((first funs) arg)] to-call (rest funs)]
         (if (empty? to-call)
           res
           (recur (conj res ((first to-call) arg)) (rest to-call)))))
    ([arg1 arg2 & args]
       (loop [res [(apply (first funs) (list* arg1 arg2 args))] to-call (rest funs) ]
         (if (empty? to-call)
           res
           (recur (conj res (apply (first to-call) (list* arg1 arg2 args))) (rest to-call)))))))

;; problem 60
(defn jd-reduce [fun fir & arg]
  (fn be-lazy [val narg]
    (lazy-seq (cons (fun (first narg) val) (be-lazy (fun (first narg) val) (rest narg)))))
  fir (rest arg))

;; problem 60 WORKING!
(defn to-red
  ([fun args] (to-red fun (first args) (rest args)))
  ([fun first-arg args]
     (if (empty? args)
       [first-arg]
       (cons first-arg (lazy-seq (to-red fun (fun first-arg (first args)) (rest args)))))
     ))

(defn to-map [ks vs]
  (if (or (empty? vs) (empty? ks))
    {}
    (merge {(first ks) (first vs)} (to-map (rest ks) (rest vs))) ))

(defn foo [fun args]
  (lazy-seq (cons (first args) (foo (fun (first args) (rest args))))))

(defn huh [fun arg1 arg2]
  (fun arg1 arg2))

(defn iter8 [f arg]
  (cons arg (lazy-seq (f arg) (iter8 f (f arg)))))

(defn groupit [f args]
  (loop [m {} a args]
    (println a)
    (if (empty? a)
      m
      (let [v (f (first a))]
        (recur (assoc m v (conj (get m v []) (first a)) ) (rest a))))))

(defn lcm [& args]
  (let [m (apply max args)]
    (loop [n m]
      (if (zero? (reduce (fn [sum y] (+ sum (mod n y))) 0 args))
        n
        (recur (+ n m))))))

(defn gcd [x y]
  (quot (* x y)
        ((fn [a b]
           (loop [n (if (> a b) a b)]
             (if (zero? (+ (mod n a) (mod n b))) n (recur (inc n))))) x y) ))

(defn jdtype [s]
  (cond
   (= 1 (:go (conj s [:go 1])) ) :map
   (= (conj s 1 1) (conj s 1)) :set
   (= 2 (last (conj s 1 2))) :vector
   true :list))

(defn jdprimes [n]
  "generate n primes as a list"
  (loop [primes [2] num (dec n)]
    (if (zero? num)
      primes
      (recur (conj primes ((fn next-p [ps i]
                             (if (not-any? #(zero? (mod i %)) ps)
                               i
                               (next-p ps (inc i)))) primes (inc (last primes)))) (dec num)) )))

(defn jdmw [f & args]
  (loop [final (first args) maps (rest args)]
    (if (empty? maps)
      final
      (recur ((fn mmerge [m new]
                (if (empty? new)
                  m
                  (let [[k v] (first new)]
                    (if (contains? m k)
                      (mmerge (assoc m k (f (get m k) v)) (rest new))
                      (mmerge (assoc m k v) (rest new)))))) final (first maps)) (rest maps)))))

(defn ttt [r]
  (let [f (fn [n] (map #(nth % n) r))
        win (fn [k row] (every? #(= k %) row))
        diag (fn [rows]
               (loop [drow [] i 0 rrows rows]
                 (if (= i 3)
                   drow
                   (recur (conj drow (nth (first rrows) i)) (inc i) (rest rrows)))))
        ]
    (let [rows  (concat r (map f (range 3)) [(diag r) (diag (map reverse r))]) ]
      (cond
       ((complement not-any?) #(win :o %) rows) :o
       ((complement not-any?) #(win :x %) rows) :x
       true nil
       true nil
       ))))

(defn psquares [string]
  (let [psq (fn [n]
              (loop [i 1]
                (cond
                 (= n (* i i)) true
                 (< n (* i i)) false
                 :else (recur (inc i)))))
        nums (map #(Integer/parseInt %) (re-seq #"\d+" string))
        tstr (fn [s] (apply str (interpose "," s)) )]
    (tstr (filter psq nums))))

(defn psq2 [string]
  (let [psq (fn [n]
              (loop [i 1]
                (cond
                 (= n (* i i)) true
                 (< n (* i i)) false
                 :else (recur (inc i)))))]
    (->> string
         (re-seq #"\d+")
         (map #(Integer/parseInt %))
         (filter psq)
         (interpose ",")
         (apply str))))

;; (#(= % (round (pow (round (sqrt %)) 2))) 25)


(defn huh [& args]
  (and
   (not (nil? (some #(= true %) args)))
   (not-every? #(= true %) args)))

(defn tot [max] max  )


(defn perfect? [n]
  (loop [i 2 sum 1]
    (if (< (quot n 2) i)
      (= n sum)
      (if (mod n i)
        (recur (inc i) (+ sum i))
        (recur (inc i) (sum))))))

(defn coprimes [num]
  (if (= 1 num)
    1
    (count (let [factors
                 (fn [n] (set (cons n (rest (filter #(= 0 (mod n %)) (range 1 (inc (quot n 2)))))) ))]
             (let [num-factors (factors num)
                   rest-factors (map factors (range 1 num))]
               (filter #(empty? (clojure.set/intersection num-factors %)) rest-factors)
               )))))

;; (zipmap (map #(re-seq #"." %) words) words)
;; (merge-with (fn [l r] (conj #{l} r)) left right)

(defn anagrams [v]
  (let [words
        (zipmap (map #(re-seq #"." %) v) (map #(conj #{} %) v))
        matcher
        (fn [w]
          (loop [res {} to-merge w]
            (if (empty? to-merge)
              res
              (let [{k 0, v 1} (first to-merge)]
                (recur
                 (merge-with
                  (fn [l r] (clojure.set/union l r))
                  res
                  (hash-map (sort k) v))
                 (rest to-merge)))
              ))

          )]
    (set (filter #(< 1 (count %))(vals (matcher words))))))

(defn tramp
  ([fun]
     (let [ret (fun)]
       (if (fn? ret)
         (tramp ret)
         ret)))
  ([fun & args]
     (let [ret (apply fun args)]
       (if (fn? ret)
         (tramp ret)
         ret)))
  )

(defn min-tr
  ([rows] (let [rev-rows (reverse rows)]
            (min-tr (first rev-rows) (rest rev-rows))) )
  ([sums rows]
     (if (empty? rows)
       (first sums)
       (let [current (first rows)
             proc-row (fn prow [row sums]
                        (loop [new-sums [] curr-row row curr-sums sums]
                          (if (empty? curr-row)
                            new-sums
                            (recur (conj new-sums (+ (first curr-row) (apply min (take 2 curr-sums)))) (rest curr-row) (rest curr-sums)))
                          )
                        )]
         (min-tr (proc-row (first rows) sums) (rest rows))
         ))))


(def triangle [
               [3]
               [2 4]
               [1 9 3]
               [9 9 2 4]
               [4 6 6 7 8]
               [5 7 3 5 1 4]])

(defn intsec [s1 s2]
  (set (keep #(get s2 %) s1)) )

;; (defn edit-distance [a b]
;;   (if (or (empty? a) (empty? b))
;;     (max (count a) (count b))
;;     (let [[fa & ra] a [fb & rb] b]
;;       (if (= fa fb)
;;         (edit-distance ra rb)
;;         (inc (min (edit-distance ra rb)
;;                   (edit-distance a rb)
;;                   (edit-distance ra b)))))))


(defn word-path
  ([words]
     (not (nil? (some #(= true %) (map #(word-path % (disj words %)) words)))))
  ([current-word words]
     (letfn [ (edit-distance [a b]
                (if (or (empty? a) (empty? b))
                  (max (count a) (count b))
                  (let [[fa & ra] a [fb & rb] b]
                    (if (= fa fb)
                      (edit-distance ra rb)
                      (inc (min (edit-distance ra rb)
                                (edit-distance a rb)
                                (edit-distance ra b)))))))]
       (if (empty? words)
         true
         (some #(word-path % (disj words %))
               (filter #(= 1 (edit-distance current-word %)) words)) ))
     ))

(defn t-closure [relations]
  (let [lefts (set (map first relations))
        left-map (zipmap (map first relations) relations)
        right-map (zipmap (map last relations) relations)]
    (println lefts)
    (println left-map)
    (println right-map)
    (map #([(right-map %) (left-map %)]) (filter #(lefts %) (keys right-map)))))


(defn t2 [relations]
  (let [rights (zipmap (map last relations) (map first relations))
        sorted-keys (reverse (sort (keys rights)))
        ]
    (letfn [(build-rels [k orig-k]
              (if (rights k)
                (conj (build-rels (rights k) orig-k) [(rights k) orig-k])
                #{}))]
      (println rights)
      (println sorted-keys)
      (reduce clojure.set/union (map #(build-rels % %) sorted-keys)))))


(defn happy [i]
  (loop [n i past-nums #{}]
    (if (= 1 n)
      true
      (if (past-nums n)
        false
        (recur (reduce + (map #(* % %) (map #(Integer. (str %) ) (str n))))
               (conj past-nums n))))))

(defn pow-set [s]
  (let [split-set (vec (map #(conj #{} %) s)) ]
    (loop [pset (conj split-set #{}) current (first split-set) rest-set (rest split-set)]
      (if ((set pset) s)
        (set pset)
        (recur (concat pset (map #(clojure.set/union current %) (disj (set pset) current)))
               (first rest-set)
               (rest rest-set))
        ))))

(defn sym-diff [s1 s2]
  (clojure.set/union (clojure.set/difference s1 s2) (clojure.set/difference s2 s1))
  )

(defn prod [n1 n2]
  (map (comp read-string str) (seq (str (* n1 n2)))) )

;; (defn lcm [& args]
;;   (println (zipmap args args)))

(defn camelize [s]
  (let [words (re-seq #"[a-zA-Z]+" s)]
    (apply str (cons (first words)
                     (map #(str (.toUpperCase (subs % 0 1)) (subs % 1))
                          (rest words))))))

;; 134
(defn null-val [k m]
  (and (contains? m k) (nil? (k m))))

;; 107
(defn pow-fun [n]
  (fn [x] (int (Math/pow x n))))

;; 122
(defn read-bin [in]
  (loop [digits (reverse in)
         values (take (count in) (iterate (partial * 2) 1))
         sum 0]
    (if (empty? digits)
      sum
      (recur
       (rest digits)
       (rest values)
       (+ sum (* (first values) (if (= \1 (first digits)) 1 0 )))))
    )
  )


(defn foo []
  (letfn [(twox [n] (* n 2))]
    (lazy-seq 1 (twox 1))))

;; 90
(defn cart-prod [l r]
  (letfn [(combine [item]
            (map #(vector item %) r))]
    (loop [coll [] lefts l]
      (if (empty? lefts)
        (set coll)
        (recur (concat coll (combine (first lefts))) (rest lefts))))
    ))


;; 128
(defn cards [pair]
  (let [suits {\D :diamond \H :heart \C :club \S :spade}
        ranks {\A 12 \K 11 \Q 10 \J 9 \T 8 \9 7 \8 6 \7 5 \6 4 \5 3 \4 2 \3 1 \2 0}]
    {:suit (suits (first pair))
     :rank (ranks (last pair))}))

;; 97
(defn pascal-row [r]
  (letfn [(calc [c prev]
            (* prev (/ (- r c) c)))]
    (loop [row [1] cols (range 1 r)]
      (if (empty? cols)
        row
        (recur (conj row (calc (first cols) (last row))) (rest cols))))))

;; 120
(defn ssq [coll]
  (letfn [(ssq [n]
            (loop [s 0 val n]
              (if (zero? val) s (recur (+ s (Math/pow (mod val 10) 2)) (quot val 10)))))]
    (count (filter #(< % (ssq %)) coll))))

;; cross product
(reduce #(+ %1 (apply * %2)) 0 (partition 2 (interleave [1 2] [3 4])))

;; 118 (timing out...)
(defn mmap [f c]
  (loop [out (list (f (first c))) in (rest c)]
    (if (empty? in)
      (reverse  out)
      (recur (cons (f (first in)) out) (rest in)))))

;; reimplement map
(defn mmap [f c]
  (lazy-seq
   (when-let [s (seq c)]
     (cons (f (first s)) (mmap f (rest s))))))

;; simple infix calculator
(defn calc [x op y & args]
  (if (empty? args)
    (op x y)
    (apply calc (cons (op x y) args))))

;; 95 -- is a binary tree?
(defn tree? [node]
  (if (nil? node)
    true
    (and (seq? node) (= 3 (count node)) (tree? (nth node 1)) (tree? (nth node 2)))))

(tree? nil)
(tree? '(:foo nil nil))

;; 93 - partially flatten a sequence

;; 147 - pascal's trapezoid
(defn ptrap [row]
  (lazy-seq
   (cons row
         (ptrap (vec (map + (cons 0 row) (conj row 0)))) )))

;; 104 roman numerals
(defn rn [i]
  (let [nums (sorted-map-by > 1000 "M" 900 "CM" 500 "D" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")]
    (loop [rem i roman []]
      (if (zero? rem)
        (apply str roman)
        (let [dec-by (first (drop-while #(< rem %) (keys nums))) ]
          (recur (- rem dec-by) (conj roman (nums dec-by))))))))

(defn bmap [v]
  (loop [m {} s v]
    (println m)
    (println s)
    (if (empty? s)
      m
      (recur (assoc m (first s) (take-while (complement keyword?) (rest s)))
             (drop-while (complement keyword?) (rest s))))))

;;137 - digits and bases
(defn digits [num base]
  (loop [digs [] n num]
      (if (< n base)
        (cons n digs)
        (recur (cons (rem n base) digs) (quot n base)))))

;; 96 - tree symmetry
(defn symm [t]
  (letfn [(treerev [t]
            (if (nil? t)
              t
              [(first t) (treerev (last t)) (treerev (second t))]))]
    (if (nil? t)
      true
      (= (second t) (treerev (last t))))))

;; 148 - big divide
(defn bdiv [n div1 div2]
  (letfn [(summult [i] (if (> 2 i) i (quot (* i (inc i)) 2)))
          (divsum [i] (* i (summult (quot (dec n) i))))]
    (- (+ (divsum div1) (divsum div2)) (divsum (* div1 div2)))))

;; 115 - balance of N
(defn balance [n]
  (let [nstr (str n) nsize (quot (count nstr) 2) 
        f (fn [s] (reduce #(+ (Integer/parseInt (str %2)) %1) 0 s))]
    (= (f (take-last nsize nstr)) (f (take nsize nstr)))))


;; 146 - trees to tables
(defn ttot [tr]
  (into {} (for [[k v] tr]
             (reduce #(conj %1 {[k (key %2)] (val %2)}) {} v))))

;; better solution...  whooooops
#(into {} (for [[a b] % [x y] b] {[a x] y}))

;; 98 - equivalence classes
(defn ecs [f s]
  (merge-with
   clojure.set/union
   (map #(hash-map (f %) #{%}) s)))

(defn ecs2 [f s]
  (vals (apply merge-with
   clojure.set/union
   (map #(hash-map (f %) #{%}) s))))

(defn ecs3 [f s]
  (set (map set (vals (group-by f s)))))

;; 108 - lazy searchin'
(defn lazy-search [& seqs]
  (let [firsts (map first seqs)]
    (if (apply = firsts)
      (first firsts)
      (recur (map (fn [s] (drop-while #(> (apply max firsts) %) s)) seqs)))))

