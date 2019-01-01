(ns day8.solution
  (:require [clojure.test :refer [is]]
            [clojure.string :as str]))

(defn get-data
  []
  (map read-string (str/split (slurp "src/day8/input.txt") #" ")))

(defn get-metadata
  [quant index data]
  [(nth data index)
   (nth data (inc index))])

(defn parse-node
  [quantity start-idx potential-data]
  (loop [nodes []]
    (let [number-children (nth potential-data start-idx)
          number-metadata (nth potential-data (inc start-idx))]
      (loop [children []
             idx (+ 2 start-idx)]
        (if (= number-children (count children))
          [{:children children
            :metadata (get-metadata number-metadata idx potential-data)}
           idx]
          (recur []))))))
          ;recur with parse-node which should return child and next index