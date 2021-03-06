; http://www.4clojure.com/problem/[number]
(use '[clojure.test :as t])

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

(defn number53 [xs]
  "Given a vector of integers, find the longest consecutive sub-sequence of increasing numbers. If two sub-sequences have the same length, use the one that occurs first. An increasing sub-sequence must have a length of 2 or greater to qualify."
  (let [reduce-fn #(let [m (:max %) c (:candidate %)]
                    (if (< (last c) %2)
                      (if (< (count m) (inc (count c)))
                        (assoc % :max (conj c %2) :candidate (conj c %2))
                        (assoc % :candidate (conj c %2)))
                      (assoc % :candidate [%2])))]
    (:max (reduce reduce-fn {:max [], :candidate [(first xs)]} (rest xs)))))

(defn number54 [n col]
  "Write a function which returns a sequence of lists of x items each. Lists of less than x items should not be returned. (can't use partition or partition-all)"
  (loop [m [] col col]
    (if (< (count col) n)
      m
      (recur (conj m (take n col)) (drop n col)))))

(defn number55 [xs]
  "Write a function which returns a map containing the number of occurences of each distinct item in a sequence."
  (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} xs))

(defn number56 [xs]
  "Write a function which removes the duplicates from a sequence. Order of the items must be maintained."
  (reduce #(if-not (some #{%2} %1) (conj %1 %2) %1) [] xs))

(defn number58 [& fns]
  "Write a function which allows you to create function compositions. The parameter list should take a variable number of functions, and create a function applies them from right-to-left. (can't use comp)"
  (fn [& xs]
    (first (reduce #(vector (apply %2 %)) xs (reverse fns)))))

(defn number59 [& fs]
  "Take a set of functions and return a new function that takes a variable number of arguments and returns a sequence containing the result of applying each function left-to-right to the argument list. (can't use juxt)"
  (fn [& args]
    (map #(apply % args) fs)))

(defn number60
  "Write a function which behaves like reduce, but returns each intermediate value of the reduction. Your function must accept either two or three arguments, and the return sequence must be lazy. (can't use reductions)"
  {:test (fn []
           (is (= [0 1 3 6 10] (take 5 (number60 + (range 10)))))
           (is (= [[1] [1 2] [1 2 3] [1 2 3 4]] (number60 conj [1] [2 3 4])))
           (is (= 120 (reduce * 2 [3 4 5]) (last (number60 * 2 [3 4 5]))))
           )}
  ([f xs] (number60 f (first xs) (rest xs)))
  ([f p xs]
   (if (empty? xs)
     [p]
     (lazy-cat [p] (number60 f (f p (first xs)) (rest xs))))))


(defn number61 [a b]
  "Write a function which takes a vector of keys and a vector of values and constructs a map from them."
  (apply hash-map (interleave a b)))

(defn number62 [f i]
  "Given a side-effect free function f and an initial value x write a function which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc. (can't use iterate)"
  (cons i (lazy-seq (number62 f (f i)))))

(defn number63 [f s]
  "Given a function f and a sequence s, write a function which returns a map. The keys should be the values of f applied to each item in s. The value at each key should be a vector of corresponding items in the order they appear in s. (can't use group-by)"
  (reduce #(assoc %1 (f %2) (concat (%1 (f %2)) [%2])) {} s))

(defn number65
  "Write a function which takes a collection and returns one of :map, :set, :list, or :vector - describing the type of collection it was given.
  You won't be allowed to inspect their class or use the built-in predicates like list? - the point is to poke at them and understand their behavior."
  {:test (fn []
           (is (= :map (number65 {:a 1, :b 2})))
           (is (= :list (number65 (range (rand-int 20)))))
           (is (= :vector (number65 [1 2 3 4])))
           (is (= :set (number65 #{10 (rand-int 5)}))))}
  [x]
  (let [c (first (str x))]
    (cond
      (= \{ c) :map
      (= \# c) :set
      (= \[ c) :vector
      :else :list)))

(defn number66 [h l]
  "GCD"
  (let [high (max h l) low (min h l)]
    (if (zero? low)
      high
      (recur low (mod high low)))))

(defn number67
  "Write a function which returns the first x number of prime numbers."
  {:test (fn []
           (is (= [2 3] (number67 2)))
           (is (= [2 3 5 7 11] (number67 5)))
           (is (= 541 (last (number67 100)))))}
  [n]
  (letfn [(prime? [x]
                  (->> (range 2 (int (inc (Math/sqrt x))))
                       (some #(= 0 (mod x %)))
                       (not)))]
    (->> (range)
         (drop 2)
         (filter prime?)
         (take n))))

(defn number69
  "Write a function which takes a function f and a variable number of maps. Your function should return a map that consists of the rest of the maps conj-ed onto the first. If a key occurs in more than one map, the mapping(s) from the latter (left-to-right) should be combined with the mapping in the result by calling (f val-in-result val-in-latter) (can't use merge-with)"
  {:test (fn []
           (is (= {:a 4, :b 6, :c 20})
               (number69 * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5}))
           (is (= {1 7, 2 10, 3 15})
               (number69 - {1 10, 2 20} {1 3, 2 10, 3 15}))
           (is (= {:a [3 4 5], :b [6 7], :c [8 9]}
                  (number69 concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]}))))}
  [f & colls]
  (reduce
    #(reduce
      (fn [m [k v]]
        (assoc m k (if-let [p (m k)]
                     (f p v)
                     v)))
      %1 %2)
    colls))

(defn number70 [s]
  "Write a function that splits a sentence up into a sorted list of words. Capitalization should not affect sort order and punctuation should be ignored."
  (sort-by clojure.string/upper-case (re-seq #"\w+" s)))

(defn number74
  "Given a string of comma separated integers, write a function that returns a new comma separated string that only contains the numbers that are perfect squares."
  {:test (fn []
           (is (= "4,9" (number74 "4,5,6,7,8,9")))
           (is (= "16,25,36" (number74 "15,16,25,36,37"))))}
  [s]
  (->> (re-seq #"\d+" s)
       (map #(Integer/parseInt %))
       (filter #(== (-> % Math/sqrt int) (Math/sqrt %)))
       (interpose ",")
       (apply str)))

(defn number75
  "Two numbers are coprime if their greatest common divisor equals 1. Euler's totient function f(x) is defined as the number of positive integers less than x which are coprime to x. The special case f(1) equals 1. Write a function which calculates Euler's totient function."
  {:test (fn []
           (is (= 1 (number75 1)))
           (is (= (count [1 3 7 9]) 4 (number75 10)))
           (is (= 16 (number75 40)))
           (is (= 60 (number75 99))))}
  [n]
  (letfn [(gcd [x y]
               (if (<= y 0)
                 x
                 (recur y (mod x y))))]
    (if (= 1 n)
      1
      (->> (range 1 n)
           (filter #(= 1 (gcd % n)))
           (count)))))

(defn number77
  "Write a function which finds all the anagrams in a vector of words. A word x is an anagram of word y if all the letters in x can be rearranged in a different order to form y. Your function should return a set of sets, where each sub-set is a group of words which are anagrams of each other. Each sub-set should have at least two words. Words without any anagrams should not be included in the result."
  {:test (fn []
           (is (= #{#{"meat" "team" "mate"}} (number77 ["meat" "mat" "team" "mate" "eat"])))
           (is (= #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}} (number77 ["veer" "lake" "item" "kale" "mite" "ever"]))))}
  [ws]
  (->> ws
       (group-by sort)
       (map #(set (val %)))
       (filter #(> (count %) 1))
       set))

(defn number80
  "A number is 'perfect' if the sum of its divisors equal the nubmer itself. 6 is a perfect number because 1+2+3=6. Write a function which returns true for perfect numbers and false otherwise."
  {:test (fn []
           (is (false? (number80 7)))
           (is (false? (number80 500)))
           (is (true? (number80 6)))
           (is (true? (number80 8128)))
           (is (true? (number80 496))))}
  [n]
  (->> (range 1 (inc (/ n 2)))
       (filter #(zero? (mod n %)))
       (apply +)
       (= n)))


(defn number81 [a b]
  "Write a function which returns the intersection of two sets. The intersection is the sub-set of items that each set has in common."
  (set (filter a b)))

(defn number83 [& x]
  "Write a function which takes a variable number of booleans. Your function should return true if some of the parameters are true, but not all of the parameters are true. Otherwise your function should return false."
  (-> (group-by identity x) count (= 2)))

(defn number85
  "Write a function which generates the power set of a given set. The power set of a set x is the set of all subsets of x, including the empty set and x itself."
  {:test (fn []
           (is (= #{#{1 :a} #{:a} #{} #{1}} (number85 #{1 :a})))
           (is (= #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}} (number85 #{1 2 3}))))}
  [s]
  (reduce (fn [m i]
            (into m (map #(conj % i) m)))
          #{#{}} s))

(defn number86
  "Happy numbers are positive integers that follow a particular formula: take each individual digit, square it, and then sum the squares to get a new number. Repeat with the new number and eventually, you might get to a number whose squared sum is 1. This is a happy number. An unhappy number (or sad number) is one that loops endlessly. Write a function that determines if a number is happy or not."
  {:test (fn []
           (is (= true (number86 7)))
           (is (= true (number86 986543210)))
           (is (= false (number86 2)))
           (is (= false (number86 3))))}
  [n]
  (letfn [(sum-square-parts [n] (->> (str n)
                                     (map (comp #(* % %) read-string str))
                                     (apply +)))]
    (loop [n n hist #{}]
      (let [s (sum-square-parts n)]
        (cond
          (hist n) false
          (= 1 s) true
          :else (recur s (conj hist n)))))))

(defn number88 [xs ys]
  "Write a function which returns the symmetric difference of two sets. The symmetric difference is the set of items belonging to one but not both of the two sets."
  (clojure.set/union (clojure.set/difference xs ys) (clojure.set/difference ys xs)))

(defn number90 [xs ys]
  "Write a function which calculates the Cartesian product of two sets."
  (set
    (for [x xs
          y ys]
      [x y])))

(defn number93
  "Write a function which flattens any nested combination of sequential things (lists, vectors, etc.), but maintains the lowest level sequential items. The result should be a sequence of sequences with only one level of nesting."
  {:test (fn []
           (is (= [["Do"] ["Nothing"]] (number93 [["Do"] ["Nothing"]])))
           (is (= [[:a :b] [:c :d] [:e :f]] (number93 [[[[:a :b]]] [[:c :d]] [:e :f]])))
           (is (= '((1 2) (3 4) (5 6)) (number93 '((1 2) ((3 4) ((((5 6))))))))))}
  [xs]
  (mapcat #(if (sequential? (first %)) (number93 %) [%]) xs))

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

(defn number97
  "Write a function which returns the nth row of Pascal's Triangle."
  {:test (fn []
           (is (= [1] (number97 1)))
           (is (= [[1]
                   [1 1]
                   [1 2 1]
                   [1 3 3 1]
                   [1 4 6 4 1]] (map number97 (range 1 6))))
           (is (= [1 10 45 120 210 252 210 120 45 10 1] (number97 11))))}
  [n]
  (let [pascal (iterate #(concat [1]
                                 (map + % (rest %))
                                 [1])
                        '(1))]
    (nth pascal (dec n))))

(defn number98
  "A function f defined on a domain D induces an equivalence relation on D, as follows: a is equivalent to b with respect to f if and only if (f a) is equal to (f b). Write a function with arguments f and D that computes the equivalence classes of D with respect to f."
  {:test (fn []
           (is (= (number98 #(* % %) #{-2 -1 0 1 2})) #{#{0} #{1 -1} #{2 -2}})
           (is (= (number98 #(rem % 3) #{0 1 2 3 4 5})
                  #{#{0 3} #{1 4} #{2 5}}))
           (is (= (number98 identity #{0 1 2 3 4})
                  #{#{0} #{1} #{2} #{3} #{4}}))
           (is (= (number98 (constantly true) #{0 1 2 3 4})
                  #{#{0 1 2 3 4}})))}
  [f D]
  (->> (reduce #(merge-with clojure.set/union % {(f %2) #{%2}}) {} D)
       (vals)
       (into #{})))

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

(defn number102
  "When working with java, you often need to create an object with fieldsLikeThis, but you'd rather work with a hashmap that has :keys-like-this until it's time to convert. Write a function which takes lower-case hyphen-separated strings and converts them to camel-case strings."
  {:test (fn []
           (is (= "something" (number102 "something")))
           (is (= "multiWordKey" (number102 "multi-word-key")))
           (is (= "leaveMeAlone" (number102 "leaveMeAlone"))))}
  [s]
  (let [ws (clojure.string/split s #"-")]
    (str (first ws)
         (apply str (map #(clojure.string/capitalize %) (rest ws))))))


(defn number103
  "Given a sequence S consisting of n elements generate all k-combinations of S, i. e. generate all possible sets consisting of k distinct elements taken from S. The number of k-combinations for a sequence is equal to the binomial coefficient."
  {:test (fn []
           (is (= #{#{4} #{5} #{6}} (number103 1 #{4 5 6})))
           (is (= #{} (number103 10 #{4 5 6})))
           (is (= (number103 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}}))
           (is (= (number103 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                                               #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}}))
           (is (= (number103 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}}))
           (is (= (number103 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                                            #{:a "abc"} #{:a "efg"} #{"abc" "efg"}})))}
  [n s]
  (->> s
       (reduce (fn [m x]
                 (reduce #(conj % (conj %2 x)) m m))
               #{#{}})
       (filter #(= (count %) n))
       (into #{})))

(defn number104
  "This is the inverse of Problem 92, but much easier. Given an integer smaller than 4000, return the corresponding roman numeral in uppercase, adhering to the subtractive principle."
  {:test (fn []
           (is (= "I" (number104 1)))
           (is (= "XXX" (number104 30)))
           (is (= "IV" (number104 4)))
           (is (= "CXL" (number104 140)))
           (is (= "DCCCXXVII" (number104 827)))
           (is (= "MMMCMXCIX" (number104 3999)))
           (is (= "XLVIII" (number104 48))))}
  [n]
  (reduce
    (fn [m [k v]] (clojure.string/replace m (apply str (repeat k "I")) v))
    (apply str (repeat n "I"))
    (array-map 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")))

(defn number105
  "Given an input sequence of keywords and numbers, create a map such that each key in the map is a keyword, and the value is a sequence of all the numbers (if any) between it and the next keyword in the sequence."
  {:test (fn []
           (is (= {} (number105 [])))
           (is (= {:a [1]} (number105 [:a 1])))
           (is (= {:a [1], :b [2]} (number105 [:a 1 :b 2])))
           (is (= {:a [1 2 3], :b [], :c [4]} (number105 [:a 1 2 3 :b :c 4]))))}
  [xs]
  (let [kvs (reduce #(if (keyword? %2)
                      (assoc %1 :ks (conj (:ks %1) %2) :vs (conj (:vs %1) []))
                      (assoc-in %1 [:vs (dec (count (:vs %1)))] (conj (last (:vs %1)) %2)))
                    {:ks [] :vs []} xs)]
    (zipmap (:ks kvs) (:vs kvs))))

(defn number107 [n]
  "Given a positive integer n, return a function (f x) which computes x^n."
  #(reduce * (repeat n %)))

(defn number108
  "Given any number of sequences, each sorted from smallest to largest, find the smallest single number which appears in all of the sequences. The sequences may be infinite, so be careful to search lazily."
  {:test (fn []
           (is (= 3 (number108 [3 4 5])))
           (is (= 4 (number108 [1 2 3 4 5 6 7] [0.5 3/2 4 19])))
           (is (= 7 (number108 (range) (range 0 100 7/6) [2 3 5 7 11 13])))
           (is (= 64 (number108 (map #(* % % %) (range))    ;; perfect cubes
                                (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
                                (iterate inc 20)))))}
  [xs & ys]
  (let [candidate (first xs)
        in-all (->> ys
                    (mapcat #(take-while (fn [x] (<= x candidate)) %))
                    (filter #(= % candidate))
                    (#(= (count %) (count ys))))]
    (if (true? in-all)
      candidate
      (recur (rest xs) (map #(drop-while (fn [x] (<= x candidate)) %) ys)))))

(defn number110
  "Write a function that returns a lazy sequence of 'pronunciations' of a sequence of numbers. A pronunciation of each element in the sequence consists of the number of repeating identical numbers and the number itself. For example, [1 1] is pronounced as [2 1] ('two ones'), which in turn is pronounced as [1 2 1 1] ('one two, one one')."
  {:test (fn []
           (is (= [[1 1] [2 1] [1 2 1 1]] (take 3 (number110 [1]))))
           (is (= [3 1 2 4] (first (number110 [1 1 1 4 4]))))
           (is (= [1 1 1 3 2 1 3 2 1 1] (nth (number110 [1]) 6)))
           (is (= 338 (count (nth (number110 [3 2]) 15)))))}
  [xs]
  (let [parts (partition-by identity xs)
        pronunciation (mapcat #(vector (count %) (first %)) parts)]
    (lazy-cat [pronunciation] (number110 pronunciation))))

(defn number114
  "Write a function which accepts an integer n, a predicate p, and a sequence. It should return a lazy sequence of items in the list up to, but not including, the nth item that satisfies the predicate."
  {:test (fn []
           (is (= [2 3 5 7 11 13] (number114 4 #(= 2 (mod % 3))
                                             [2 3 5 7 11 13 17 19 23])))
           (is (= ["this" "is" "a" "sentence"] (number114 3 #(some #{\i} %)
                                                          ["this" "is" "a" "sentence" "i" "wrote"])))
           (is (= ["this" "is"] (number114 1 #{"a"}
                                           ["this" "is" "a" "sentence" "i" "wrote"]))))}
  [n p xs]
  (when-let [[x & xs] xs]
    (let [n (if (p x) (dec n) n)]
      (if (> n 0)
        (lazy-seq (cons x (number114 n p xs)))))))

(defn number115
  "A balanced number is one whose component digits have the same sum on the left and right halves of the number. Write a function which accepts an integer n, and returns true iff n is balanced."
  {:test (fn []
           (is (true? (number115 11)))
           (is (true? (number115 121)))
           (is (false? (number115 123)))
           (is (true? (number115 0)))
           (is (false? (number115 88099)))
           (is (true? (number115 89098)))
           (is (true? (number115 89089)))
           (is (= [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101] (take 20 (filter #(number115 %) (range)))))
           )}
  [n]
  (let [ns (map int (str n))
        c (count (str n))]
    (= (apply + (take (/ c 2) ns))
       (apply + (drop (/ (dec c) 2) ns)))))

(defn number116
  {:test (fn []
           (is (false? (number116 4)))
           (is (true? (number116 563)))
           (is (= 1103 (nth (filter number116 (range)) 15))))}
  [n]
  (letfn [(prime? [x]
                  (if (= 1 x)
                    false
                    (->> (range 2 (int (inc (Math/sqrt x))))
                         (some #(= 0 (mod x %)))
                         (not))))
          (mean [a b] (/ (+ a b) 2))]
    (if (or (not (prime? n)) (< n 3))
      false
      (let [next-prime (->> (range (inc n) Integer/MAX_VALUE) (filter prime?) (first))
            last-prime (->> (range (dec n) 1 -1) (filter prime?) (first))]
        (= (mean last-prime next-prime) n)))))

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

(defn number121
  "Given a mathematical formula in prefix notation, return a function that calculates the value of the formula. The formula can contain nested calculations using the four basic mathematical operators, numeric constants, and symbols representing variables. The returned function has to accept a single parameter containing the map of variable names to their values. "
  {:test (fn []
           (is (= 2 ((number121 '(/ a b))
                      '{b 8 a 16})))
           (is (= 8 ((number121 '(+ a b 2))
                      '{a 2 b 4})))
           (is (= [6 0 -4]
                  (map (number121 '(* (+ 2 a)
                                      (- 10 b)))
                       '[{a 1 b 8}
                         {b 5 a -2}
                         {a 2 b 11}])))
           (is (= 1 ((number121 '(/ (+ x 2)
                                    (* 3 (+ y 1))))
                      '{x 4 y 1}))))}
  [l]
  (fn [smap]
    (let [full-smap (merge smap {'+ +, '/ /, '* *, '- -})]
      (letfn [(my-eval [[f & xs]]
                       (apply (full-smap f) (map #(if (coll? %)
                                                   (my-eval %)
                                                   (get full-smap % %))
                                                 xs)))]
        (my-eval l)))))

(defn number122 [binary]
  "Convert a binary number, provided in the form of a string, to its numerical value."
  (let [t (->> (reverse binary)
               (map-indexed #(vector % (* 2 (read-string (str %2)))))
               (reduce #(+ %1 (Math/pow (last %2) (first %2))) 0)
               (int))]
    (if (= \1 (last binary)) t (dec t))))


(defn number131
  "Given a variable number of sets of integers, create a function which returns true iff all of the sets have a non-empty subset with an equivalent summation."
  {:test (fn []
           (is (true? (number131 #{-1 1 99}
                                 #{-2 2 888}
                                 #{-3 3 7777})))
           (is (false? (number131 #{1}
                                  #{2}
                                  #{3}
                                  #{4})))
           (is (true? (number131 #{1})))
           (is (false? (number131 #{1 -3 51 9}
                                  #{0}
                                  #{9 2 81 33})))
           (is (true? (number131 #{1 3 5}
                                 #{9 11 4}
                                 #{-3 12 3}
                                 #{-3 4 -2 10}))))}
  [& xs]
  (let [permutate (fn [s]
                    (reduce (fn [m x]
                              (reduce #(conj % (conj %2 x)) m m))
                            #{#{}} s))
        sums (map (fn [x]
                    (->> (permutate x)
                         (remove empty?)
                         (map #(reduce + %))
                         (into #{})))
                  xs)]
    (not (empty? (apply clojure.set/intersection sums)))))

(defn number132
  "Write a function that takes a two-argument predicate, a value, and a collection; and returns a new collection where the value is inserted between every two items that satisfy the predicate."
  {:test (fn []
           (is (= '(1 :less 6 :less 7 4 3) (number132 < :less [1 6 7 4 3]))))}
  ([f x xs] (number132 f x xs nil))
  ([f x xs last]
   (if-not (empty? xs)
     (lazy-cat
       (if (and (not (nil? last)) (f last (first xs)))
         [x (first xs)]
         [(first xs)])
       (number132 f x (rest xs) (first xs))))))

(defn number135 [i & r]
  "Write a function that accepts a variable length mathematical expression consisting of numbers and the operations +, -, *, and /. Assume a simple calculator that does not do precedence and instead just calculates left to right."
  (reduce (fn [m [f v]] (f m v))
          i (partition 2 r)))

(defn number137
  "Write a function which returns a sequence of digits of a non-negative number (first argument) in numerical system with an arbitrary base (second argument). Digits should be represented with their integer values, e.g. 15 would be [1 5] in base 10, [1 1 1 1] in base 2 and [15] in base 16. "
  {:test (fn []
           (is (= [1 2 3 4 5 0 1] (number137 1234501 10)))
           (is (= [0] (number137 0 11)))
           (is (= [1 0 0 1] (number137 9 2)))
           (is (= [1 0] (let [n (rand-int 100000)] (number137 n n))))
           (is (= [16 18 5 24 15 1] (number137 Integer/MAX_VALUE 42))))}
  [n base]
  (loop [r [] c n]
    (if (< c base)
      (cons c r)
      (recur (cons (rem c base) r) (quot c base)))))

(defn number143 [a b]
  "Create a function that computes the dot product of two sequences. You may assume that the vectors will have the same length."
  (->> (map * a b)
       (reduce +)))

(defn number144
  "Write an oscillating iterate: a function that takes an initial value and a variable number of functions. It should return a lazy sequence of the functions applied to the value in order, restarting from the first function after it hits the end."
  {:test (fn []
           (is (= [3.14 3 3.0] (take 3 (number144 3.14 int double))))
           (is (= [3 0 5 2 7] (take 5 (number144 3 #(- % 3) #(+ 5 %)))))
           (is (= [0 1 0 1 0 1 2 1 2 1 2 3] (take 12 (number144 0 inc dec inc dec inc)))))}
  [n & fs]
  (cons n (lazy-seq
            (apply number144 ((first fs) n) (concat
                                              (rest fs)
                                              [(first fs)])))))

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

(defn number148
  "Write a function which calculates the sum of all natural numbers under n (first argument) which are evenly divisible by at least one of a and b (second and third argument). Numbers a and b are guaranteed to be coprimes."
  {:test (fn []
           (is (= 0 (number148 3 17 11)))
           (is (= 23 (number148 10 3 5)))
           (is (= 233168 (number148 1000 3 5)))
           (is (= "2333333316666668" (str (number148 100000000 3 5))))
           (is (= "110389610389889610389610" (str (number148 (* 10000 10000 10000) 7 11))))
           (is (= "1277732511922987429116" (str (number148 (* 10000 10000 10000) 757 809))))
           (is (= "4530161696788274281" (str (number148 (* 10000 10000 1000) 1597 3571)))))}
  [n a b]
  (letfn [(partial-sum [n a1 an] (-> (+' a1 an) (*' n) (/ 2)))
          (find-sum [x]
                    (let [q (quot (dec n) x)]
                      (partial-sum q x (* x q))))]
    (- (+ (find-sum a) (find-sum b))
       (find-sum (* a b)))))

(defn number153 [s]
  "Given a set of sets, create a function which returns true if no two of those sets have any elements in common1 and false otherwise. Some of the test cases are a bit tricky, so pay a little more attention to them."
  (apply distinct? (reduce #(concat % %2) s)))

(defn number156 [default xs]
  "Write a function which takes a default value and a sequence of keys and constructs a map."
  (zipmap xs (repeat default)))

(defn number158 [f]
  "Write a function that accepts a curried function of unknown arity n. Return an equivalent function of n arguments. "
  (fn [& x] (reduce #(% %2) f x)))

(defn number166 [f a b]
  (cond (f a b) :lt
        (f b a) :gt
        :else :eq))

(t/run-tests)

