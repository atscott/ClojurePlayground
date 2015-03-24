; http://www.4clojure.com/problem/[number]

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
  (->> (repeat (count xs) delim)
       (interleave xs)
       (butlast)))
