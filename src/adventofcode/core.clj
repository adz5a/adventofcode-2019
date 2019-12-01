(ns adventofcode.core
  (:require [clojure.string :as string]))

(defn get-fuel-qty
  [module-mass]
  (-> module-mass
      (/ 3)
      Math/floor
      int
      (- 2)))

(defn get-necessary-fuel
  [fuel-mass]
  (loop [fuel-mass fuel-mass
         additional-fuel 0]
    (let [required-fuel (get-fuel-qty fuel-mass)]
      (if (pos? required-fuel)
        (recur required-fuel
               (+ additional-fuel required-fuel))
        additional-fuel))))

(defn get-fuel-qty'
  [module-mass]
  (let [module-fuel (get-fuel-qty module-mass)
        necessary-fuel (get-necessary-fuel module-fuel)]
    (+ module-fuel necessary-fuel)))

(def answer1 (let [data (slurp "src/adventofcode/input1.txt")
                   data (string/split data #"\n")
                   data (map (fn [line] (Integer/parseInt line)) data)]
               ;; result 3256794
               (reduce + 0
                       (map get-fuel-qty data))))

(def answer2 (let [data (slurp "src/adventofcode/input1.txt")
                   data (string/split data #"\n")
                   data (map (fn [line] (Integer/parseInt line)) data)]
               (reduce + 0
                       (map get-fuel-qty' data))))
