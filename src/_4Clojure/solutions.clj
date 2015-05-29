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

(defn number55 [xs]
  "Write a function which returns a map containing the number of occurences of each distinct item in a sequence."
  (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} xs)
  )

(defn number61 [a b]
  "Write a function which takes a vector of keys and a vector of values and constructs a map from them."
  (apply hash-map (interleave a b)))

(defn number62 [f i]
  "Given a side-effect free function f and an initial value x write a function which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc. (can't use iterate)"
  (cons i (lazy-seq (number62 f (f i)))))

(defn number63 [f s]
  "Given a function f and a sequence s, write a function which returns a map. The keys should be the values of f applied to each item in s. The value at each key should be a vector of corresponding items in the order they appear in s. (can't use group-by)"
  (reduce #(assoc %1 (f %2) (concat (%1 (f %2)) [%2])) {} s))

(defn number66 [h l]
  "GCD"
  (let [high (max h l) low (min h l)]
    (if (zero? low)
      high
      (recur low (mod high low)))))

(defn number81 [a b]
  "Write a function which returns the intersection of two sets. The intersection is the sub-set of items that each set has in common."
  (set (filter a b)))

(defn number83 [& x]
  "Write a function which takes a variable number of booleans. Your function should return true if some of the parameters are true, but not all of the parameters are true. Otherwise your function should return false."
  (-> (group-by identity x) count (= 2)))

(defn number88 [xs ys]
  "Write a function which returns the symmetric difference of two sets. The symmetric difference is the set of items belonging to one but not both of the two sets."
  (clojure.set/union (clojure.set/difference xs ys) (clojure.set/difference ys xs)))

(defn number90 [xs ys]
  "Write a function which calculates the Cartesian product of two sets."
  (set
    (for [x xs
          y ys]
      [x y])))

(defn number95 [t]
  "Write a predicate which checks whether or not a given sequence represents a binary tree. Each node in the tree must have a value, a left child, and a right child."
  (cond
    (= nil t) true
    (not (coll? t)) false
    :else (cond (and
                  (= 3 (count t))
                  (number95 (second t))
                  (number95 (last t))) true
                :else false)))

(defn number96 [[_ left right]]
  "Let us define a binary tree as 'symmetric' if the left half of the tree is the mirror image of the right half of the tree. Write a predicate to determine whether or not a given binary tree is symmetric. "
  (letfn [(mirror-leaves [[h l r :as t]]
                         (if (coll? t)
                           [h (mirror-leaves r) (mirror-leaves l)]
                           t))]
    (= left (mirror-leaves right))))

(defn number97 [n]
  "Write a function which returns the nth row of Pascal's Triangle."
  (loop [rowNum 2 previous [1]]
    (if (> rowNum n)
      previous
      (recur
        (inc rowNum)
        (reduce
          #(conj % (+ (nth previous (dec %2) 0) (nth previous %2 0)))
          []
          (range rowNum))))))

(defn number99 [x y]
  "Write a function which multiplies two numbers and returns the result as a sequence of its digits."
  (->> (* x y)
       str
       (re-seq #"\d")
       (map #(Integer. %))))

(defn number100
  "Write a function which calculates the least common multiple. Your function should accept a variable number of positive integers or ratios. "
  ([a b]
   (letfn [(gcd [x y]
                (if (<= y 0)
                  x
                  (recur y (mod x y))))]
     (* a (quot b (gcd a b)))))
  ([a b & c]
   (reduce #(number100 % %2) a (cons b c))))

(defn number107 [n]
  "Given a positive integer n, return a function (f x) which computes x^n."
  #(reduce * (repeat n %)))

(defn number118 [f xs]
  "Map is one of the core elements of a functional programming language. Given a function f and an input sequence s, return a lazy sequence of (f x) for each element x in s."
  (if (empty? xs)
    []
    (lazy-cat [(f (first xs))] (number118 f (rest xs)))))


(defn number120 [xs]
  "Write a function which takes a collection of integers as an argument. Return the count of how many elements are smaller than the sum of their squared component digits. For example: 10 is larger than 1 squared plus 0 squared; whereas 15 is smaller than 1 squared plus 5 squared."
  (letfn [(sum-square-components [n]
                                 (reduce #(+ % (Math/pow (Integer/parseInt (str %2)) 2))
                                         0 (str n)))]
    (count (filter #(< % (sum-square-components %)) xs))))

(defn number122 [binary]
  "Convert a binary number, provided in the form of a string, to its numerical value."
  (let [t (->> (reverse binary)
               (map-indexed #(vector % (* 2 (read-string (str %2)))))
               (reduce #(+ %1 (Math/pow (last %2) (first %2))) 0)
               (int))]
    (if (= \1 (last binary)) t (dec t))))

(defn number135 [i & r]
  "Write a function that accepts a variable length mathematical expression consisting of numbers and the operations +, -, *, and /. Assume a simple calculator that does not do precedence and instead just calculates left to right."
  (reduce (fn [m [f v]] (f m v))
          i (partition 2 r)))

(defn number143 [a b]
  "Create a function that computes the dot product of two sequences. You may assume that the vectors will have the same length."
  (->> (map * a b)
       (reduce +)))

(defn number146 [m]
  "For this problem, your goal is to 'flatten' a map of hashmaps. Each key in your output map should be the 'path' that you would have to take in the original map to get to a value, so for example {1 {2 3}} should result in {[1 2] 3}. You only need to flatten one level of maps: if one of the values is a map, just leave it alone."
  (into {} (for [k (keys m)
                 v (m k)]
             {[k (key v)] (val v)})))

(defn number147 [r]
  "Write a function that, for any given input vector of numbers, returns an infinite lazy sequence of vectors, where each next one is constructed from the previous following the rules used in Pascal's Triangle. For example, for [3 1 2], the next row is [3 4 3 2]."
  (lazy-cat
    [r]
    (number147 (reduce
                 #(conj % (+' (nth r (dec %2) 0) (nth r %2 0)))
                 []
                 (range (inc (count r)))))))

(defn number153 [s]
  "Given a set of sets, create a function which returns true if no two of those sets have any elements in common1 and false otherwise. Some of the test cases are a bit tricky, so pay a little more attention to them."
  (apply distinct? (reduce #(concat % %2) s)))

(defn number156 [default xs]
  "Write a function which takes a default value and a sequence of keys and constructs a map."
  (zipmap xs (repeat default)))

(defn number166 [f a b]
  (cond (f a b) :lt
        (f b a) :gt
        :else :eq))

