(ns day1.solution
  (:require [clojure.test :refer [is]]
            [clojure.string :as str]))

(defn get-lines
  []
  (str/split-lines (slurp "src/day1/input.txt")))

(defn find-frequency
  [frequency-changes]
  (->> frequency-changes
       (map read-string)
       (apply +)))

(defn find-first-repeated-frequency
  {:test (fn []
           (is (= (find-first-repeated-frequency ["+7" "+7" "-2" "-7" "-4"]) 14)))}
  [frequency-changes]
  (loop [reached-frequencies #{0}
         current-freq 0
         index 0]
    (let [new-freq (+ (read-string (nth frequency-changes (mod index (count frequency-changes)))) current-freq)]
      (if (contains? reached-frequencies new-freq)
        new-freq
        (recur (conj reached-frequencies new-freq)
               new-freq
               (inc index))))))

(comment
  (find-frequency (get-lines))
  (find-first-repeated-frequency (get-lines)))