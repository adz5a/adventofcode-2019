(ns adventofcode.day3
  (:require [clojure.string :as string]))

(defn string->paths
  [path-string]
  (let [path-string (string/split path-string #",")
        path (map
               #(let [[_ direction length] (re-matches #"(R|L|U|D)(\d+)" %)]
                  [direction (Integer/parseInt length)])
               path-string)]
    path))

(def paths (let [input-str (-> (slurp "src/adventofcode/input3.txt")
                              (string/split #"\n"))]
            (map string->paths input-str)))

(defn process-move
  [move position]
  (let [[direction length] move
        [x y] position]
    (case direction
      "L" (map
            vector
            (range (- x length) x)
            (repeat y))
      "R" (map
            vector
            (range x (+ x length 1))
            (repeat y))
      "U" (map
            vector
            (repeat x)
            (range y (+ y length 1)))
      "D" (map
            vector
            (repeat x)
            (range (- y length) y)))))

(defn path->positions
  [path]
  (let [origin [0 0]]
    (loop [positions #{origin}
           position origin
           path path]
      (if-let [move (first path)]
        (recur (apply conj positions (process-move move position))
               (let [[direction length] move
                     [x y] position]
                 (case direction
                   "R" [(+ x length) y]
                   "L" [(- x length) y]
                   "U" [x (+ y length)]
                   "D" [x (- y length)]))
               (next path))
        positions))))

(defn dist
  [position]
  (let [[x y] position]
    (+ (Math/abs x) (Math/abs y))))

(comment
  (let [wire-1 (path->positions (string->paths "R75,D30,R83,U83,L12,D49,R71,U7,L72"))
        wire-2 (path->positions (string->paths "U62,R66,U55,R34,D71,R55,D58,R83"))]
    (filter wire-1 wire-2))


  (process-move ["U" 1] [1 0])


  (let [wire-1 (path->positions (string->paths "R8,U5"))
        wire-2 (path->positions (string->paths "U7,R6,D4,L4"))]
    (println wire-1)
    (println wire-2)
    (filter wire-1 wire-2))

  (let [wire-1 (path->positions (string->paths "R1,U1,L1,D1"))]
    wire-1)


  (process-move ["U" 10] [0 0])
  (process-move ["D" 10] [0 0])
  (process-move ["R" 10] [0 0])
  (process-move ["L" 10] [0 0]))

