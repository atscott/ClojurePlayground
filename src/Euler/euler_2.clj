; *https://projecteuler.net/problem=2
(ns euler-2)

(defn fib [a b]
  (lazy-cat (cons a (fib b (+ a b)))))

(def fib2
  (cons 1 (cons 1 (lazy-seq (map + fib2 (rest fib2))))))

(def fib3
  (lazy-cat [1 1] (map + fib3 (rest fib3))))

(defn euler2 [fib-seq]
  (reduce +
          (filter even?
                  (take-while #(< % 4000000) fib-seq))))

(euler2 (fib 1 1))
(euler2 fib2)
(euler2 fib3)
