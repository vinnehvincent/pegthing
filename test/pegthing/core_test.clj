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

(defn row-num
  "Returns row number the position the poistion belongs to"
  [pos]
  (inc (count (take-while #(> pos %) tri) ))
  )

(defn connect-down-left
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ row pos)
        destination (+ 1 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn connect-down-right
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ 1 row pos)
        destination (+ 2 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn connect-right
  [board max-pos pos]
  (let [neighbor (inc pos)
        destination (inc neighbor)]
    (if-not (or (triangular? neighbor) (triangular? pos))
      (connect board max-pos pos neighbor destination)
      board))
  )

(defn add-pos
  "Pegs the position and adds connectons"
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connection-creation-fn]
              (connection-creation-fn new-board max-pos pos))
            pegged-board
            [connect-right connect-down-right connect-down-left])))

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
    (is (= {1 {:connections {4 2}}
            4 {:connections {1 2}}} (connect {} 15 1 2 4))))
  (testing "Row num a postion belongs to"
    (are [x y] (= x y)
               1 (row-num 1)
               3 (row-num 5)))
  (testing "Adding connections"
    (testing "Connect down left"
      (is (= {1 {:connections {4 2}}
              4 {:connections {1 2}}} (connect-down-left {} 15 1))))
    (testing "Connect down right"
      (is (= {3  {:connections {10 6}}
              10 {:connections {3 6}}} (connect-down-right {} 15 3))))
    (testing "Connect right"
      (is (= {4 {:connections {6 5}}
              6 {:connections {4 5}}} (connect-right {} 15 4)))))
  (testing "Adding a position and it's connections to the board"
    (is (={1 {:connections {6 3, 4 2}, :pegged true}
           4 {:connections {1 2}}
           6 {:connections {1 3}}} (add-pos {} 15 1)))))