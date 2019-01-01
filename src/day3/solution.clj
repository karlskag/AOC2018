(ns day3.solution
  (:require [clojure.test :refer [is]]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn get-lines
  []
  (str/split-lines (slurp "src/day3/input.txt")))

(defn parse-claims-data
  {:test (fn []
           (is (= (parse-claims-data "#1 @ 1,3: 5x4") {:id          "1"
                                                       :top-left-xy [1 3]
                                                       :width       5
                                                       :height      4})))}
  [data]
  {:id          (last (re-find #"\#(.*?) \@" data))
   :top-left-xy (vec (map read-string (str/split (last (re-find #"\@ (.*?)\:" data)) #",")))
   :width       (read-string (last (re-find #"\: (.*?)x" data)))
   :height      (read-string (subs (re-find #"x.*" data) 1))})

(defn calculate-covered-coordinates
  {:test (fn []
           (is (= (calculate-covered-coordinates {:id          "1"
                                                  :top-left-xy [1 3]
                                                  :width       2
                                                  :height      2})
                  #{[1 3] [2 3] [1 4] [2 4]})))}
  [claim-data]
  (->> (:top-left-xy claim-data)
       (repeat (:width claim-data))
       (map-indexed (fn [index [x y]]
                      [(+ x index) y]))                     ;cols
       (repeat (:height claim-data))
       (map-indexed (fn [index row-coords]
                      (map (fn [[x y]] [x (+ index y)]) row-coords)))
       (apply concat)
       (into #{})))

(defn find-overlapping-squares
  {:test (fn []
           (is (= (find-overlapping-squares ["#1 @ 1,3: 4x4"
                                             "#2 @ 3,1: 4x4"
                                             "#3 @ 5,5: 2x2"])
                  #{[3 3] [4 3] [3 4] [4 4]})))}
  [unparsed-claims-data]
  (->> unparsed-claims-data
       (map parse-claims-data)
       (map calculate-covered-coordinates)
       (apply concat)
       (frequencies)
       (filter (fn [[k v]] (> v 1)))
       (keys)
       (set)))

(defn find-non-overlapping-claim-id
  {:test (fn []
           (is (= (find-non-overlapping-claim-id ["#1 @ 1,3: 4x4"
                                                  "#2 @ 3,1: 4x4"
                                                  "#3 @ 5,5: 2x2"])
                  "3")))}
  [unparsed-claims-data]
  (let [overlapping-squares (find-overlapping-squares unparsed-claims-data)]
    (->> unparsed-claims-data
         (map parse-claims-data)
         (filter (fn [data] (empty? (set/intersection
                                      overlapping-squares
                                      (calculate-covered-coordinates data)))))
         (first) ;Use (some) above?
         (:id))))

(comment
  (count (find-overlapping-squares (get-lines)))
  (find-non-overlapping-claim-id (get-lines)))


