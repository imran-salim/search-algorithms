; Name: Imran Salim
;   ID: 1241622

(ns assignment2.core
  (:require [clojure.string :refer [split-lines]])
  (:require [clojure.data.priority-map :refer [priority-map]])
  (:gen-class)
  (:import (java.lang Math))
  (:import (java.lang String)))

;(def testMap {:map ["+-----+"
;                    "|XSX X|"
;                    "|X   X|"
;                    "|X   X|"
;                    "|XX  X|"
;                    "|XX GX|"
;                    "+-----+"]
;              :path []})

(def verbose true)

(defn read-map-from-file
  "Read a map from a text file.
   Takes a text file as the argument."
  [map-file]
  {:map (split-lines (slurp map-file))
   :path []})

(defn print-state
  "Print the map to the terminal.
   Takes a state as the argument."
  [state]
  (doseq [line (:map state)]
          (println line)))

(defn start
  "Return the co-ordinates of the start position in the map.
   Takes a state as the argument."
  [state]
  (let [current-map (:map state)]
    ; Return the first item in the collection returned by the for loop.
    (first
      ; Loop through the rows and columns of the map.
      (for [y (range (count current-map))
            x (range (count (first current-map)))
            ; Bind the char at the position of x and y and check if it equals a capital "S".
            :let [c (get (get current-map y) x)]
            :when (= c \S)]
        [x y]))))

(defn goal
  "Return the co-ordinates of the goal position in the map.
   Takes a state as the argument."
  [state]
    (let [current-map (:map state)]
      ; Return the first item in the collection returned by the for loop.
      (first
        ; Loop through the rows and columns of the map.
        (for [y (range (count current-map))
              x (range (count (first current-map)))
              ; Bind the char at the position of x and y and check if it equals a capital "G".
              :let [c (get (get current-map y) x)]
              :when (= c \G)]
          [x y]))))

(defn next-position
  "Return the co-ordinates of the next position in the map.
   Takes a position and a direction as arguments and then passes it in to the second set of parameters as an x, y, and direction."
  ([pos dir] (next-position (first pos) (second pos) dir))
  ([x y dir]
   ; Depending on the direction provided, a modified vector is returned.
   (case dir
     :north [x (- y 1)]
     :south [x (+ y 1)]
     :east  [(+ x 1) y]
     :west  [(- x 1) y])))

(defn position
  "Returns the current position in the map.
   Takes a state and then passes in the path of the state, and the start position on the map to the second set of parameters as path and a position."
  ([state]
   (position (:path state) (start state)))
  ([path pos]
   ; If the path is empty, then return the current position. Otherwise call the function again with the tail of the path and the next position.
   (if (empty? path) pos
     (recur (rest path) (next-position pos (first path))))))

(defn cost
  "Calculates the cost of the path given by counting the number of items in the path collection.
   Takes a state as an argument."
  [state]
  (count (:path state)))

(defn heuristic
  "Calculates the Euclidian distances from the current position to the goal.
   Takes a state as the argument."
  [state]
  (let [current-position (position state)
        goal-position (goal state)]
    ; Use the Euclidian distance formula to calculate the heuristic value.
    (Math/sqrt
      (+ (Math/pow (- (get current-position 0) (get goal-position 0)) 2)
         (Math/pow (- (get current-position 1) (get goal-position 1)) 2)))))

(defn expand
  "Expand the state from the current position and draw dots on the map on the positions expanded to.
   Takes a state as the argument."
  [state]
  (let [pos (position state)
        current-map (:map state)]
    ; For every possible direction,
    (for [dir [:north :east :south :west]
          ; Concatenate the direction to the existing path and bind it to new-path.
          :let [new-path (conj (:path state) dir)
                ; Get the next position as a vector of co-ordiantes.
                [x y] (next-position pos dir)
                ; Access the char at the position and check if it is the goal.
                c (get (get current-map y) x)
                ; If it is goal, then return the current map, otherwise clone the current map with a dot drawn on the next position, but only when,
                new-map (if (= c \G) current-map
                          (assoc current-map y (apply str (assoc (vec (nth current-map y)) x \.))))]
          ; the next position is an empty space or is goal, signifying that the next position is a valid place to move.
          :when (some? (some #{c} [\space \G]))]
      ; Add the state to a list.
      {:map new-map :path new-path})))

(defn search
  "Search for the goal in a given map.
   Takes a function and state as arguments."
  [func state]
  ; Start a loop with the frontier, already traversed nodes, and an expansion count.
  (loop [frontier (priority-map state (func state))
         already-travesed {(start state) 0}
         expansion-count 0]
    ; Get the state which is the key from the node at the front of the queue, and if it is the goal, print out the state, number of expansions, and the cost.
    (let [s (first (peek frontier))]
      (if (= (position s) (goal s))
        (do
          (print-state s)
          (println "Number of expansions:" expansion-count)
          (println "Cost of path:" (cost s)))
        ; Otherwise, generate a list which contains a nested vector from expanding the first node in the queue.
        ; The first vector contains the expanded state and its value (heuristic or heuristic + cost), and the second vector contains the position of the expanded state
        ; and the position of the previous node.
        (let [expanded-states
              (for [expanded-state (expand s)
                    ; Keep track of already traversed nodes, and only insert nodes if the the previous node does not exist, or the node being checked has a value less than the previous node.
                    :let [current-func-val (func expanded-state)
                          pos (position expanded-state)
                          prev (get already-travesed pos)]
                    :when (or (nil? prev) (< current-func-val prev))]
                [[expanded-state current-func-val][pos current-func-val]])]
          (print-state expanded-states)
          ;(when verbose (print-state s))
          ; Disregard the node in the front of the queue, and place the expanded states and their values into the frontier.
          (recur (into (pop frontier) (map first expanded-states))
                 ; Place the posi13/15tions and their current values (heuristic or heuristic + cost) into the already traversed collection.
                 (into already-travesed (map second expanded-states))
                 ; Increment the counter for number of expansions.
                 (inc expansion-count)))))))

(defn best-first
  "Searches the state for a goal using the best-first search algorithm.
   Takes a text file as the argument."
  [map-file]
  (search heuristic (read-map-from-file map-file)))

(defn a-star
  "Searches the state for a goal using the A* search algorithm.
   Takes a text file as the argument."
  [map-file]
  ; An anonymous function is used to sum the heuristic and cost.
  (search #(+ (heuristic %) (cost %)) (read-map-from-file map-file)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;(best-first "/home/is39/comp316/assignment2/resources/map2.txt"))
  (a-star "/home/is39/comp316/assignment2/resources/map2.txt"))
