(ns pegthing.core-test
  (:require [clojure.test :refer :all]
            [pegthing.core :refer :all]))
(deftest pegthing-test
  (testing "Tri function"
    (is (= '(1 3 6 10 15) (take 5 tri))))
  (testing "Triangular?"
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
           6 {:connections {1 3}}} (add-pos {} 15 1))))
  (testing "Create a new board"
    (is (= {1  {:pegged true, :connections {6 3, 4 2}},
            2  {:pegged true, :connections {9 5, 7 4}},
            3  {:pegged true, :connections {10 6, 8 5}},
            4  {:pegged true, :connections {13 8, 11 7, 6 5, 1 2}},
            5  {:pegged true, :connections {14 9, 12 8}},
            6  {:pegged true, :connections {15 10, 13 9, 4 5, 1 3}},
            7  {:pegged true, :connections {9 8, 2 4}},
            8  {:pegged true, :connections {10 9, 3 5}},
            9  {:pegged true, :connections {7 8, 2 5}},
            10 {:pegged true, :connections {8 9, 3 6}},
            11 {:pegged true, :connections {13 12, 4 7}},
            12 {:pegged true, :connections {14 13, 5 8}},
            13 {:pegged true, :connections {15 14, 11 12, 6 9, 4 8}},
            14 {:pegged true, :connections {12 13, 5 9}},
            15 {:pegged true, :connections {13 14, 6 10}},
            :rows 5} (new-board 5)))))

(deftest moving-pegs
  (testing "Is a position pegged?"
    (let [board (new-board 15)]
      (is (= true (pegged? board 1)))))
  (testing "Remove a peg from a position"
    (let [board (new-board 15)]
      (is (= false (pegged? (remove-peg board 1) 1)))))
  (testing "Place a peg at a position"
    (let [board (remove-peg (new-board 15) 4)]
      (is (= true (pegged? (place-peg board 4) 4)))))
  (testing "Move a peg from one position to another"
    (let [board (remove-peg (new-board 15) 4)
          new-board (move-peg board 1 4)]
      (is (= false (pegged? new-board 1)))
      (is (= true (pegged? new-board 4)))))
  (testing "Valid moves on the board"
    (let [board (new-board 15)]
         (is (= {} (valid-moves board 1)))
         (is (= {4 2} (valid-moves (remove-peg board 4) 1)))
         (is (= {4 2 6 3}
                (valid-moves (remove-peg (remove-peg board 4) 6) 1)))))
  (testing "whether a move is valid"
    (let [board (remove-peg (new-board 15) 4) ]
      (is (= 2 (valid-move? board 1 4)))
      (is (= nil (valid-move? board 8 4)))))
  (testing "making a move"
    (let [board (remove-peg (new-board 15) 4)
          new-board (make-move  board 1 4)]
      (is (= true (pegged? new-board 4)))
      (is (= false (pegged? new-board 1)))
      (is (= false (pegged? new-board 2)))))
  (testing "does a board have a possible moves"
    (let [board (new-board 15)]
      (is (= nil (can-move? board)))
      (is (= {4 2} (can-move? (remove-peg board 4)))))))


(deftest characters-as-strings-test
  (testing "characters-as-strings"
    (are
      [x y]
      (= x y)
       '("a" "c" "b") (characters-as-strings "a cb")
      '("a" "b") (characters-as-strings "a     b"))))

(deftest letter->pos-test
  (testing "letter to position"
    (are
      [x y]
      (= x y)
      1 (letter->pos "a")
      21 (letter->pos "u"))))
