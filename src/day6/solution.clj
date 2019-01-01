(ns day6.solution
  (:require [clojure.test :refer [is]]
            [clojure.string :as str]))

(defn get-lines
  []
  (str/split-lines (slurp "src/day6/input.txt")))

(defn create-coords
  {:test (fn []
           (is (= (create-coords ["242, 164" "455, 200"])
                  (quote ((242 164) (455 200))))))}
  [lines]
  (->> lines
       (map (fn [val]
              (map read-string
                   (str/split val #", "))))))

(defn calc-manhattan-distance
  {:test (fn []
           (is (= (calc-manhattan-distance '(2 3) '(5 6)) 6)))}
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

(defn calc-distance-to-nodes
  {:test (fn []
           (is (= (calc-distance-to-nodes '(0 0) (quote ((2 2) (4 4))))
                  {'(2 2) 4
                   '(4 4) 8})))}
  [point nodes]
  (reduce (fn [acc val]
            (assoc acc val (calc-manhattan-distance point val))) {} nodes))

(defn closest-or-nil
  {:test (fn []
           (is (= (closest-or-nil '([(3 4) 3] [(2 2) 4]))
                  '(3 4)))
           (is (= (closest-or-nil '([(2 3) 4] [(3 2) 4]))
                  nil)))}
  [[[key1 dist1] [key2 dist2]]]
  (if (= dist1 dist2)
    nil
    (if (< dist1 dist2)
      key1
      key2)))

(defn get-closest-node-or-nil
  {:test (fn []
           (is (= (get-closest-node-or-nil '(0 0) (quote ((2 2) (4 4))))
                  '(2 2))))}
  [point nodes]
  (->> (calc-distance-to-nodes point nodes)
       (sort-by val)
       (take 2)
       (closest-or-nil)))

(defn minmax-xy
  [minmax xy nodes]
  (apply minmax (flatten (map xy nodes))))

(defn is-edge-node
  {:test (fn []
           (is (is-edge-node '(1 6) (create-coords ["1, 1" "1, 6" "8, 3" "3, 4" "5, 5" "8, 9"])))
           (is (is-edge-node '(1 1) (create-coords ["1, 1" "1, 6" "8, 3" "3, 4" "5, 5" "8, 9"])))
           (is (is-edge-node '(8 9) (create-coords ["1, 1" "1, 6" "8, 3" "3, 4" "5, 5" "8, 9"])))
           (is (not (is-edge-node '(5 5) (create-coords ["1, 1" "1, 6" "8, 3" "3, 4" "5, 5" "8, 9"]))))
           (is (not (is-edge-node '(3 4) (create-coords ["1, 1" "1, 6" "8, 3" "3, 4" "5, 5" "8, 9"])))))}
  [[x y] nodes]
  (or (= x (minmax-xy max first nodes))
      (= x (minmax-xy min first nodes))
      (= y (minmax-xy max second nodes))
      (= y (minmax-xy min second nodes))))

(defn contains-edge-node
  [points nodes]
  (not-empty (filter (fn [point]
                       (is-edge-node point nodes)) points)))

(defn create-system
  {:test (fn []
           (is (= (create-system '(3 3))
                  '([0 0] [0 1] [0 2] [0 3]
                     [1 0] [1 1] [1 2] [1 3]
                     [2 0] [2 1] [2 2] [2 3]
                     [3 0] [3 1] [3 2] [3 3]))))}
  [[end-x end-y]]
  (for [x (range (+ 2 end-x)) y (range (+ 2 end-y))]
    [x y]))

(defn solve-a
  {:test (fn []
           (is (= (solve-a (create-coords ["1, 1" "1, 6" "8, 3" "3, 4" "5, 5" "8, 9"])) 17)))}
  [nodes]
  (->> (create-system [(minmax-xy max first nodes)
                       (minmax-xy max second nodes)])
       (reduce (fn [acc coord]
                 (let [closest (get-closest-node-or-nil coord nodes)]
                   (if (nil? closest)
                     acc
                     (if (nil? (get acc closest))
                       (assoc acc closest [coord])
                       (update acc closest conj coord)))))
               {})
       (filter (fn [[k v]] (not (contains-edge-node v nodes))))
       (map (fn [[k v]] (count v)))
       (apply max)))

(defn over-max-distance
  {:test (fn []
           (is (not (over-max-distance 32 [4 3] (create-coords ["1, 1" "1, 6" "8, 3" "3, 4" "5, 5" "8, 9"]))))
           (is (over-max-distance 32 [2 2] (create-coords ["1, 1" "1, 6" "8, 3" "3, 4" "5, 5" "8, 9"]))))}
  [limit point nodes]
  (->> (reduce (fn [acc node]
                      (+ (calc-manhattan-distance point node) acc)) 0 nodes)
       (<= limit)))

(defn solve-b
  {:test (fn []
           (is (= (solve-b (create-coords ["1, 1" "1, 6" "8, 3" "3, 4" "5, 5" "8, 9"]) 32) 16)))}
  [nodes limit]
  (->> (create-system [(minmax-xy max first nodes)
                       (minmax-xy max second nodes)])
       (filter (fn [point]
                 (not (over-max-distance limit point nodes))))
       (count)))

(comment
  (solve-a (create-coords (get-lines)))
  (solve-b (create-coords (get-lines)) 10000))





