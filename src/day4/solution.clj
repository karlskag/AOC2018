(ns day4.solution
  (:require [clojure.test :refer [is]]
            [clojure.string :as str]))

(defn get-lines
  []
  (str/split-lines (slurp "src/day4/input.txt")))

(defn get-sorted-input
  []
  (sort (get-lines)))

(defn is-id?
  [data]
  (.contains data "#"))

(defn parse-id
  [data]
  (last (re-find #"\#(.*?) \b" data)))

(defn is-sleep?
  [data]
  (.contains data "sleep"))

(defn parse-time
  [data]
  (Integer/parseInt (last (re-find #"\:(.*?)\]" data))))

(defn parse-sleep-data
  {:test (fn []
           (is (= (parse-sleep-data ["[1518-02-13 00:02] Guard #1021 begins shift"
                                     "[1518-02-13 00:11] falls asleep"
                                     "[1518-02-13 00:37] wakes up"
                                     "[1518-02-13 23:56] Guard #2447 begins shift"
                                     "[1518-02-14 00:30] falls asleep"
                                     "[1518-02-14 00:47] wakes up"
                                     "[1518-02-16 23:35] Guard #1021 begins shift"
                                     "[1518-02-13 00:05] falls asleep"
                                     "[1518-02-13 00:10] wakes up"])
                  {"1021" [[11 37] [5 10]]
                   "2447" [[30 47]]})))}
  [unparsed-data]
  (loop [current-officer nil
         index 0
         parsed-data {}]
    (if (= index (count unparsed-data))
      parsed-data
      (if (is-id? (nth unparsed-data index))
        (if (contains? parsed-data (parse-id (nth unparsed-data index)))
          (recur (parse-id (nth unparsed-data index))
                 (inc index)
                 parsed-data)
          (recur (parse-id (nth unparsed-data index))
                 (inc index)
                 (assoc parsed-data (parse-id (nth unparsed-data index)) [])))
        (if (is-sleep? (nth unparsed-data index))
          (recur current-officer
                 (inc index)
                 (update parsed-data current-officer conj [(parse-time (nth unparsed-data index))]))
          (recur current-officer
                 (inc index)
                 (assoc parsed-data current-officer (conj (pop (get parsed-data current-officer))
                                                          (conj (peek (get parsed-data current-officer)) (parse-time (nth unparsed-data index)))))))))))

(parse-sleep-data (get-sorted-input))

(defn calculate-minutes-slept
  {:test (fn []
           (is (= (calculate-minutes-slept [[11 13] [5 10]])
                  '(11 12 5 6 7 8 9))))}
  [parsed-data]
  (flatten (map (fn [[sleep wake]]
                  (range sleep wake))
                parsed-data)))

(defn id-of-most-minutes-slept
  {:test (fn []
           (is (= (id-of-most-minutes-slept ["[1518-02-13 00:02] Guard #1021 begins shift"
                                             "[1518-02-13 00:11] falls asleep"
                                             "[1518-02-13 00:37] wakes up"
                                             "[1518-02-13 23:56] Guard #2447 begins shift"
                                             "[1518-02-14 00:30] falls asleep"
                                             "[1518-02-14 00:47] wakes up"
                                             "[1518-02-16 23:35] Guard #1021 begins shift"
                                             "[1518-02-13 00:05] falls asleep"
                                             "[1518-02-13 00:10] wakes up"])
                  1021)))}
  [sorted-data]
  (->> (parse-sleep-data sorted-data)
       (reduce (fn [acc [k v]]
                 (if (> (count (calculate-minutes-slept v)) (second acc))
                   [k (count (calculate-minutes-slept v))]
                   acc)) ["id" 0])
       (first)
       (read-string)))

(defn highest-minute-freq-for-id
  [id sorted-input]
  (->> (get (parse-sleep-data sorted-input) (str id))
       (calculate-minutes-slept)
       (frequencies)
       (sort-by val)
       (last)))

(defn solve-a
  [input]
  (let [id (id-of-most-minutes-slept input)]
    (* id (first (highest-minute-freq-for-id id input)))))

(defn get-id-freq-min
  [input]
  (->> (parse-sleep-data input)
       (reduce (fn [acc [k v]]
                 (let [minute-frq (highest-minute-freq-for-id k input)]
                   (if (nil? minute-frq)
                     acc
                     (if (> (second minute-frq) (second acc))
                       [k (second minute-frq) (first minute-frq)]
                       acc)))) ["id" 0 1])))

(defn solve-b
  [sorted-input]
  (let [data (get-id-freq-min sorted-input)]
    (* (read-string (first data)) (last data))))

(comment
  (solve-a (get-sorted-input))
  (solve-b (get-sorted-input)))

