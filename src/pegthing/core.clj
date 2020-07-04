(ns pegthing.core
  (:require [clojure.set :as set])
  (:gen-class))

;(declare succesful-move prompt-move game-over query-rows)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
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

(defn new-board
  "Create a new board with the specicified number of rows"
  [rows]
  (let [initial-board {:rows rows}
        max-pos (row-tri rows)]
    (reduce (fn [board pos] (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))