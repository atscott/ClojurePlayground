; http://www.4clojure.com/problem/[number]

(defn number23 [xs]
  "Write a function which reverses a sequence."
  (reduce conj () xs))

(defn number24 [x] (reduce + x))

(defn number25 [x] (filter odd? x))

(defn number26 [n]
  "Write a function which returns the first X fibonacci numbers."
  (loop [n n, last 0, current 1, acc '()]
    (if (< n 1)
      acc
      (recur (dec n) current (+ current last) (concat acc `(~current))))))

(defn number27 [x]
  "Write a function which returns true if the given sequence is a palindrome."
  (if (< (count x) 2)
    true
    (if (= (first x) (last x))
      (recur (drop-last (rest x)))
      false)))

(defn number28 [xs]
  "Write a function which flattens a sequence."
  (let [x (first xs) ys (rest xs)]
    (concat
      (if (coll? x) (number28 x) [x])
      (when (not-empty ys) (number28 ys)))))

(defn number29 [s]
  "Write a function which takes a string and returns a new string containing only the capital letters."
  (apply str (re-seq #"[A-Z]" s)))


(defn number30 [xs]
  "Write a function which removes consecutive duplicates from a sequence."
  (map first (partition-by identity xs)))

(fn number32 [xs]
  "Write a function which duplicates each element of a sequence."
  (mapcat identity (map #(list % %) xs)))

(defn number32v2 [xs]
  mapcat list xs xs)

(defn number33 [xs n]
  "Write a function which replicates each element of a sequence a variable number of times."
  (apply mapcat list (repeat n xs)))

(defn number34 [l u]
  "Write a function which creates a list of all integers in a given range."
  (take (- u l) (iterate inc l)))

(defn number38 [& colls]
  "Write a function which takes a variable number of parameters and returns the maximum value."
  (last (sort colls)))

(defn number39 [& colls]
  "Write a function which takes two sequences and returns the first item from each, then the second item from each, then the third, etc."
  (apply mapcat list colls))

(defn number40 [delim xs]
  "Write a function which separates the items of a sequence by an arbitrary value. (can't use interpose)"
  (->> (repeat delim)
       (interleave xs)
       (butlast)))

(defn number41 [xs n]
  "Write a function which drops every Nth item from a sequence."
  (mapcat #(take (dec n) %) (partition-all n xs)))

(defn number42 [n]
  "Write a function which calculates factorials."
  (reduce * 1 (range 1 (inc n))))

(defn number43 [xs n]
  "Write a function which reverses the interleave process into x number of subsequences."
  (apply map list (partition-all n xs)))

(defn number44 [n xs]
  "Write a function which can rotate a sequence in either direction."
  (cond
    (< n 0) (recur (inc n) (cons (last xs) (butlast xs)))
    (> n 0) (recur (dec n) (concat (rest xs) [(first xs)]))
    :else xs))

(defn number46 [f]
  "Write a higher-order function which flips the order of the arguments of an input function."
  (fn [a b] (f b a)))

(defn number50 [xs]
  "Write a function which takes a sequence consisting of items with different types and splits them up into a set of homogeneous sub-sequences. The internal order of each sub-sequence should be maintained, but the sub-sequences themselves can be returned in any order (this is why 'set' is used in the test cases)."
  (vals (group-by type xs)))

(defn number61 [a b]
  "Write a function which takes a vector of keys and a vector of values and constructs a map from them."
  (apply hash-map (interleave a b)))

(defn number66 [h l]
  "GCD"
  (let [high (max h l) low (min h l)]
    (if (zero? low)
      high
      (recur low (mod high low)))))

(defn number83 [& x]
  "Write a function which takes a variable number of booleans. Your function should return true if some of the parameters are true, but not all of the parameters are true. Otherwise your function should return false."
  (-> (group-by identity x) count (= 2)))

(defn number156 [default xs]
  "Write a function which takes a default value and a sequence of keys and constructs a map."
  (zipmap xs (repeat default)))

(defn number166 [f a b]
  (cond (f a b) :lt
        (f b a) :gt
        :else :eq))

(defn number81 [a b]
  "Write a function which returns the intersection of two sets. The intersection is the sub-set of items that each set has in common."
  (set (filter a b)))
