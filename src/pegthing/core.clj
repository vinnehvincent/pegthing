(ns pegthing.core
  (:require [clojure.set :as set])
  (:gen-class))

(declare succesful-move prompt-move game-over query-rows)

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
  "Create a new board with the specified number of rows"
  [rows]
  (let [initial-board {:rows rows}
        max-pos (row-tri rows)]
    (reduce (fn [board pos] (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))
(defn pegged?
  "Does the position have a peg in it"
  [board pos]
  (get-in board [pos :pegged]))

(defn remove-peg
  "Take the peg at a given position out of the board"
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  "Place a peg at a given position"
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  "Take peg out initial-pos and move it to destination"
  [board initial-pos destination]
  (place-peg (remove-peg board initial-pos) destination))

(defn valid-moves
  "Return a map of valid moves for pos, where the key is the destination and
   the value is the jumped position e.g {4 2}"
  [board pos]
  (into {}
        (filter (fn [[destination jumped]]
                  (and (not (pegged? board destination))
                       pegged? board jumped))
                (get-in board [pos :connections]))))

(defn valid-move?
  "Return jumped position if move is valid or return nil"
  [board initial-pos destination]
  (get (valid-moves board initial-pos) destination))

(defn make-move
  "Move peg from initial-pos to destination, removing jumped position"
  [board initial-pos destination]
  (if-let [jumped (valid-move? board initial-pos destination)]
    (move-peg (remove-peg board jumped) initial-pos destination)))

(defn can-move?
  "Do any of the pegged positions have valid moves?"
  [board]
  (some (comp not-empty (partial valid-moves board))
        (map first (filter #(get (second %) :pegged) board))))

(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)

(def ansi-styles
  {:red "[31m"
   :green "[32m"
   :blue "[34m"
   :reset "[0m"})
(defn ansi
  "Returns string that will apply ANSI style"
  [style]
  (str \u001b (style ansi-styles)))

(defn colorize
  "Apply ansi color to text"
  [text color]
  (str (ansi color) text (ansi :reset)))

(defn render-pos
  [board pos]
  (str (nth letters (dec pos))
       (if (get-in board [pos :pegged])
         (colorize "0" :blue)
         (colorize "-" :red))))


(defn row-positions
  "Return all positions in the given row"
  [row-num]
  (range (inc (or (row-tri (dec row-num)) 0))
         (inc (row-tri row-num))))

(defn row-padding
  "String of spaces to add to the beginnning of the row to center it"
  [row-num rows]
  (let [pad-length (/ (* (- rows row-num) pos-chars) 2)]
    (apply str (take pad-length (repeat " ")))))

(defn render-row
  [board row-num]
  (str (row-padding row-num (:rows board))
       (clojure.string/join " " (map (partial render-pos board)
                                     (row-positions row-num)))))
(defn print-board
  [board]
  (doseq [row-num (range 1 (inc (:rows board)))]
    (println (render-row board row-num))))

(defn letter->pos
  "Converts letter string to corresponding position number"
  [letter]
  (inc (- (int (first letter))alpha-start)))

(defn get-input
  "Waits for the user to enter text and hit enter, then cleans enter"
  ([] (get-input nil))
  ([default]
   (let [input (clojure.string/trim (read-line))]
     (if (empty? input)
       default
       (clojure.string/lower-case input)))))
(defn characters-as-strings
  [characters]
   (map str (seq (char-array (clojure.string/replace characters #" " "")))))

(defn prompt-empty-peg
  [board]
  (println "Here's your board: ")
  (print-board board)
  (println "Remove which peg? [e]")
  (prompt-move (remove-peg board (letter->pos (get-input "e")))))

(defn prompt-rows
  []
  (println "How many rows? [5]")
  (let [rows (Integer. (get-input 5))
        board (new-board rows)]
    (prompt-empty-peg board)))

(defn game-over
  "Announces that a game is over and prompts for new game"
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println "Game over! You had " remaining-pegs " pegs")
    (print-board board)
    (println "Play again? y/n [y")
    (let [input (get-input "y")]
      (if (= "y" input)
        (prompt-rows)
        (do
          (println "Bye!")
          (System/exit 0))))))

(defn user-entered-valid-move
  "Handles the next move after the user has entered a valid move"
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))

(defn user-entered-invalid-move
  "Handles the next move after the user has enter an invalid move"
  [board]
  (println "\n!!! That was an invalid move :(\n")
  (prompt-move board))

(defn prompt-move
  [board]
  (println "\nHere's your board: ")
  (print-board board)
  (println "Move from where to where? Enter two letters: ")
  (let [input (map letter->pos (characters-as-strings (get-input)))]
    (if-let [new-board (make-move board (first input) (second input))]
      (user-entered-valid-move new-board)
      (user-entered-invalid-move board))))
