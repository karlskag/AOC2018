(ns day7.solution
  (:require [clojure.test :refer [is]]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn get-lines
  []
  (str/split-lines (slurp "src/day7/input.txt")))

(def split-test-input (str/split-lines "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin."))
(def alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn parse-input
  {:test (fn []
           (is (= (parse-input ["Step C must be finished before step A can begin."
                                "Step C must be finished before step F can begin."])
                  {"C" '("F" "A")})))}
  [split-input]
  (reduce (fn [acc line]
            (let [split-line (str/split line #" ")]
              (update acc (nth split-line 1) conj (nth split-line 7)))) {} split-input))

(defn reverse-deps
  {:test (fn []
           (is (= (reverse-deps {"C" '("F" "A")})
                  {"F" '("C")
                   "A" '("C")})))}
  [dep-chart]
  (reduce (fn [acc [name blocked]]
            (reduce (fn [map value]
                      (update map value conj name)) acc blocked)) {} dep-chart))

(defn find-unblocked-nodes
  [traversed chart]
  (filter (fn [[k v]]
            (and (not (contains? traversed k))
                 (empty? (set/difference (set v) traversed)))) chart))

(defn find-parallel-unblocked-nodes
  {:test (fn []
           (is (= (find-parallel-unblocked-nodes #{"C"} #{nil} {"F" '("C") "A" '("C") "C" '()}) [["F" '("C")] ["A" '("C")]]))
           (is (= (find-parallel-unblocked-nodes #{"C"} #{nil "A"} {"F" '("C") "A" '("C") "C" '()}) [["F" '("C")]]))
           (is (= (find-parallel-unblocked-nodes #{"C"} #{nil "A"} {"F" '("C" "A") "A" '("C") "C" '()}) [])))}
  [traversed current chart]
  (filter (fn [[k v]]
            (and (not (.contains (str/join (set/union traversed current)) (str k)))
                 (empty? (set/difference (set v) (set (map str traversed)))))) chart))

(defn traverse
  [chart]
  (reduce (fn [ordered-traversed [node blockers]]
            (let [unblocked-nodes (find-unblocked-nodes (set ordered-traversed) chart)
                  next-node (first (into (sorted-map) unblocked-nodes))]
              (conj ordered-traversed (first next-node)))) [] chart))

(defn traverse-instructions
  {:test (fn [] (is (= (traverse-instructions (parse-input split-test-input)) ["C" "A" "B" "D" "F" "E"])))}
  [dep-chart]
  (let [reversed-deps (reverse-deps dep-chart)
        start-nodes (set/difference (set (keys dep-chart)) (set (keys reversed-deps)))] ;correct way of finding start?
    (->> (reduce #(assoc % %2 '()) reversed-deps start-nodes)
         (traverse))))

(defn calc-target
  {:test (fn []
           (is (= (calc-target "G" 60) 67)))}
  [node delay]
  (if (nil? node)
    nil
    (+ delay (inc (str/index-of alphabet node)))))

(defn get-all-active
  [state modified]
  (concat (map (fn [[k val]]
                 (:working-on val)) (:workers state))
          (map (fn [[k val]]
                 (:working-on val)) modified)))


(defn update-tasks
  [chart delay state]
  (as-> (update state :workers #(reduce (fn [acc [key val]]
                                          (if (or (nil? (:working-on val))
                                                  (= (:target val) (:ticks val)))
                                            (assoc acc key {:working-on nil
                                                            :target     nil
                                                            :ticks      0
                                                            :finished   (concat (:finished val) (:working-on val))})
                                            (assoc acc key (update val :ticks inc)))) {} %)) $
        (update $ :finished #(distinct (concat % (reduce (fn [acc [key val]] (concat acc (:finished val))) [] (:workers $)))))
        (update $ :workers #(reduce (fn [acc [key val]]
                                      (if (nil? (:working-on val))
                                        (assoc acc key (as-> (update (assoc val :working-on (ffirst (into (sorted-map)
                                                                                                          (find-parallel-unblocked-nodes
                                                                                                            (set (:finished $))
                                                                                                            (set (get-all-active $ acc))
                                                                                                            chart))))
                                                                     :ticks inc) w
                                                             (assoc w :target (calc-target (:working-on w) delay))))
                                        (assoc acc key val))) {} %))))

(defn tick
  [state]
  (update state :total-elapsed inc))

(defn traverse-parallel
  {:test (fn [] (is (= (traverse-parallel {:no-workers 2 :chart (parse-input split-test-input) :delay 0}) 15)))}
  [options]
  (let [reversed-deps (reverse-deps (:chart options))
        start-nodes (set/difference (set (keys (:chart options))) (set (keys reversed-deps)))
        state {:workers       (reduce #(assoc % %2 {:working-on nil
                                                    :target     nil
                                                    :ticks      0
                                                    :finished   []}) {} (range 1 (inc (:no-workers options))))
               :finished      []
               :active        []
               :total-elapsed 0}]
    (as-> (reduce #(assoc % %2 '()) reversed-deps start-nodes) $
          (loop [new-state state]
            (let [unblocked-nodes (find-unblocked-nodes (set (:finished new-state)) $)
                  updated-state (->> new-state
                                     (update-tasks $ (:delay options))
                                     (tick))]
              (if (empty? (filter #(not (nil? %)) (get-all-active updated-state [])))
                (dec (:total-elapsed updated-state))
                (recur updated-state)))))))


(traverse-instructions (parse-input (get-lines)))
(traverse-parallel {:no-workers 5
                    :chart      (parse-input (get-lines))
                    :delay      60})
