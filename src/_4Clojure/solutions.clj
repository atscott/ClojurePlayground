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
