; http://www.4clojure.com/problem/[number]

(defn number24 [x] (reduce + x))

(defn number25 [x] (filter odd? x))

(defn number26
  "Write a function which returns the first X fibonacci numbers."
  [n]
  (loop [n n, last 0, current 1, acc '()]
    (if (< n 1)
      acc
      (recur (dec n) current (+ current last) (concat acc `(~current)))))
  )

(defn number27
  "Write a function which returns true if the given sequence is a palindrome."
  [x]
  (if (< (count x) 2)
    true
    (if (= (first x) (last x))
      (recur (drop-last (rest x)))
      false)))

(defn number28
  "Write a function which flattens a sequence."
  [x]
  (if (empty? x)
    x
    (if (coll? (first x))
      (concat (number28 (first x)) (number28 (rest x)))
      (cons (first x) (number28 (rest x)))
      )))


