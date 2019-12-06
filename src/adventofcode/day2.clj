(ns adventofcode.day2
  (:require [clojure.string :as string]))

(defn string->program
  [s]
  (let [tokens (-> s (string/replace #"\n" "")
                     (string/split #","))
        program (map #(Integer/parseInt %) tokens)]
    (vec program)))

(def input (slurp "src/adventofcode/input2.txt"))

(def program (string->program input))
(println program)

(defmulti play
  (fn [program position]
    (case (nth program position)
      1 :add
      2 :multiply
      99 :stop)))

(def operations {1 +
                 2 *})

(defn play
  [program position]
  (let [opcode (nth program position)
        operandes (let [targets [(nth program (inc position))
                                 (nth program (inc (inc position)))]]
                    [(nth program (first targets))
                     (nth program (second targets))])
        operation (operations opcode)
        new-val (operation (first operandes)
                           (second operandes))
        new-program (assoc program
                           (nth program (+ position 3))
                           new-val)]
    (println {:position position
              :opcode opcode
              :operandes operandes
              :operation operation
              :new-val new-val
              :new-program new-program})
    new-program))

(defn run
  [program]
  (loop [position 0
         program program]
    (let [opcode (nth program position)]
      (if (= 99 opcode)
        program
        (recur (+ position 4)
               (play program position))))))



(comment
  index
  partition-all
  (assoc [1] 0 2)

  (let [program [1,9,10,3,2,3,11,0,99,30,40,50]]
    (run program))
  (let [program [1,0,0,0,99]
        out (run program 0)]
    (println out))
  (let [program [2,3,0,3,99]
        out (run program 0)]
    (println out))
  (let [program [2,4,4,5,99,0]
        out (run program 0)]
    (println out))
  (let [program [1,1,1,4,99,5,6,0,99]
        out (run (assoc program
                        1 12
                        2 0))]
    (println out))

  (let [position 0]
    (loop [program program
           position 0]
      (let [new-program (play program position)
            new-position (+ position 4)
            new-opcode (nth program new-position)]
        (println new-position)
        (if (= 99 new-opcode)
          (first new-program)
          (recur new-program
                 new-position))))))
