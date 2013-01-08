(ns pandigital)

(defn pandigital? [numbers]
  "Returns true if input numbers are 1-9 pandigital. Input: list of numbers"
  (= (apply str (sort (apply str numbers))) "123456789"))

(pandigital/pandigital? '(3245)) ;false 
(pandigital/pandigital? [123 45 67 98]) ;true
(source pandigital/pandigital?)
