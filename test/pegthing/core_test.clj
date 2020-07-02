(ns pegthing.core-test
  (:require [clojure.test :refer :all]
            [pegthing.core :refer :all]))

(defn tri*
  "Generates a lazy sequence of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))
(def tri (tri*))

(defn triangular?
  "Is the number triangular i.e 1, 3, 6, 10 etc.?"
  [n]
  (= n (last (take-while #(>= n %) tri))))

(defn row-tri [n]
  "The triangular number at the end of the row"
  (last (take n tri)))

(defn connect
  "Form a mutual connection between positions"
  [board max-pos pos neighbor destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
               (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[pos destination] [destination pos]])
    board))

(deftest pegthing-test
  (testing "Tri function"
    (is (= '(1 3 6 10 15) (take 5 tri))))
  (testing "Tringaular?"
    (is (= true (triangular? 6)))
    (is (= false (triangular? 5))))
  (testing "Triangular number at the end of a row"
    (are [x y] (= x y)
               1 (row-tri 1)
               3 (row-tri 2)
               6 (row-tri 3)))
  (testing "Connections between positions"
    (is #{1 {:connections {4 2}} 4 {:connections {1 2}}} (connect {} 15 1 2 4))))