(ns day2.solution
  (:require [clojure.test :refer [is]]
            [clojure.string :as str]))

(defn get-lines
  []
  (str/split-lines (slurp "src/day2/input.txt")))

(defn get-number-of-seq-containing-letter-freq
  [letter-sequences freq]
  (reduce (fn [acc v]
            (if (contains? (set (vals (frequencies v))) freq)
              (inc acc)
              acc)) 0 letter-sequences))

(defn get-intersection
  [seq-1 seq-2]
  (remove nil? (map-indexed (fn [index val]
                              (if (= val (nth seq-2 index))
                                val
                                nil)) seq-1)))

(defn get-sibling-seq-if-present
  {:test (fn []
           (is (= (get-sibling-seq-if-present "hejsan" ["hejsan" "helsap"]) nil))
           (is (= (get-sibling-seq-if-present "fghij" ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"]) "fguij")))}
  [test-seq all-seqs]
  (loop [test-seq-2 (first all-seqs)
         next-word-index 1]
    (if (= (count (get-intersection test-seq test-seq-2)) (- (count test-seq) 1))
      test-seq-2
      (if (= next-word-index (count all-seqs))
        nil
        (recur (nth all-seqs next-word-index)
               (inc next-word-index))))))

(defn solve-b
  {:test (fn []
           (is (= (solve-b ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"]) "fgij")))}
  [letter-seqs]
  (loop [tested-seq (first letter-seqs)
         next-word-index 1]
    (if (not (nil? (get-sibling-seq-if-present tested-seq letter-seqs)))
      (str/join (get-intersection tested-seq (get-sibling-seq-if-present tested-seq letter-seqs)))
      (recur (nth letter-seqs next-word-index)
             (inc next-word-index)))))

(defn solve-a
  [letter-seqs]
  (* (get-number-of-seq-containing-letter-freq letter-seqs 2)
     (get-number-of-seq-containing-letter-freq letter-seqs 3)))

(comment
  (solve-a (get-lines))
  (solve-b (get-lines)))