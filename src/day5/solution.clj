(ns day5.solution
  (:require [clojure.test :refer [is]]
            [clojure.string :as str]))

(defn get-lines
  []
  (slurp "src/day5/input.txt"))

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn react-polymers
  {:test (fn []
           (is (= (react-polymers "vVZzWwrEeCcwvoOVsSiCHhDQqdcUuWwsiCctVvTIaAKkSCcIpPYmMgGyfFlLENneEeEeqQVNnpPVvTtHhaQeEHhiIEoOzZaeqQqgaAGdDVSsvjJDdhPpWpLlPwmLlMPzZgGpusSTtQqxXUeEUuMkvVKmLDdnNCpPVaABbOnNobBvhHcaaAAloOIipVvsSRrPiIHmMWIinEFfyBbYYyeiGgjJQqIDdQqNqQSsWwoeEtKkTtnNjEeJxRuUrXjJAaXxjYTtcAaXxCvVyqqfFQgGQmMvVsSsSJBaACMmcbAaGhHgEepPQrRqLlMmpTtyvVYIisSmMwkKJjWGgyYJjrRyDlLBbKdDkYydiHhIgKsetTEEeTtJdCtTlLoORrZyYzfFemMEJjdDyjJYuVvoOxXqQrDdROoFfFfwDdaFfAKZzkBbQnNqyYWXxxXkKvyYVMBbYymgGuUCcOodwqYJjcCyWwQWDgGUrRcpPCZzGutTQqUgXxAamJjMiIVvutWwaAuUTUkKuCccCVSspPEeqQSsveETLIiltEeUbNnBurRMnNmoOoaAOmMhHMmEesSIiQrvqQVbYykKBRqXxjJhruURUujeeEEJnNLlSsScKskgGKSkkKCSsdDuUMmsEzZeJyYYyDdjYUrRusIiSJbBbOonNXWwxgGUuekKzSsZEGgMjJBbmSJjcMmXxDtTeEdCNnsBDdjWMmwsStTwkKiIyYxXwWBtTRoONFfnrnmMNbVVEevEeejJEvSsCcsTasSSsAdDVvOxIiXfhHFdJXxjHhiIRrFfVJFfjvlLxZSszXCcAaAaSHruURSshFfshHuzZTthHhHaApPDdZXyYxzUFfDwWtXxDdeqQETTtiIdYoOOoyiIqIitUuTQgNnTtGDoOiIQqmnNOVhHvCcoLlHKDduUDkKyYdRryYlLqQkbBzZKlLUugGOokxXhNjJnuUYyMxaAdDoOJTtxXqQuUBbTcFfCtcNHhnQqCRrIitTJjqCclLQduUDcUaAuAaQqCjAurRUzZOoaxXeHhWYyaAwUubBkKNEecTtCZzsSnEYyyYCcTWwkKtlLCfvVXXxxDdSscCfjaAJtSsTFoOJjZzROorrRZaARTtYyrGkKgFfHhcCFfVyYsSvZznNTtfFUuUuYaAwWFfAaysSQqzntTJOojPzZpaAJjvVeEmMUuLlGgOonqQNccCCBbDhHAabBvSskNZznKDrRdhHdDeEHhVwTtSsaAWQcCKkqUKkwTtWuUOWwXVvxcCocCsSuqUbBhHPYynNpudDYyrRhHQfFsSdKKUukPpzZkUljJLdIiDEeRrRuYyUCcumMUrNLlntMmDdTkFgGXDAHhPpsSgGpPaxXxXdEVvTWmMSsdzgGhHPpBuUbXuUfFuUxSVNnIivPplzCcZLIyYRrmMUuICcwjJWMtTppPbBRjJrPArcCRVuNnUvdDfeicCkPbBpkKWdDyYwVvwuUBBbbWBiIbnNyYRrxXfFKrROocYyCWwILllLnNXxPpNndDsSQKkcCVvQqPpRrUuuUgGtTAaqpvrr")
                  "rwVaQEaeqWotpygKsJdCuhYwsTOxCfnUkFXETWdzSIIMAfepvrr")))}
  [text]
  (let [text-len (count text)
        new-text (str/replace text #"Aa|aA|Bb|bB|Cc|cC|Dd|dD|Ee|eE|Ff|fF|Gg|gG|Hh|hH|Ii|iI|Jj|jJ|Kk|kK|Ll|lL|Mm|mM|Nn|nN|Oo|oO|Pp|pP|Qq|qQ|Rr|rR|Ss|sS|Tt|tT|Uu|uU|Vv|vV|Ww|wW|Xx|xX|Yy|yY|Zz|zZ" "")]
    (if (= text-len (count new-text))
      new-text
      (react-polymers new-text))))

(defn find-shortest-possible
  [text]
  (reduce (fn [current-low val]
            (let [latest-val (count (react-polymers (str/replace text (re-pattern (str val "|" (str/upper-case val))) "")))]
              (if (> current-low latest-val)
                latest-val
                current-low))) 50000 alphabet))

(comment
  ;(time (remove-opposites (get-lines)))                    ;15 min
  (time (count (react-polymers (get-lines))))               ;947.245 msecs
  (find-shortest-possible (get-lines)))


;(time (find-shortest-optimized (get-lines)))


;OLD CODE BELOW

;returns collection of values not min
;(defn find-shortest-optimized
;  [text]
;  (apply min (pmap (fn [val]
;               (count (react-polymers (str/replace text (re-pattern (str val "|" (str/upper-case val))) ""))))
;             alphabet)))

;(defn reverse-case
;  {:test (fn []
;           (is (= (reverse-case "a") "A"))
;           (is (= (reverse-case "B") "b")))}
;  [char]
;  (if (= (str/upper-case char) char)
;    (str/lower-case char)
;    (str/upper-case char)))
;
;(defn drop-nth
;  [indexes indexed-coll]
;  (->> indexed-coll
;       (filter (fn [[idx val]]
;                 (not (contains? indexes idx))))
;       (map-indexed (fn [idx [i item]]
;                      [idx item]))))
;
;(defn remove-opposites
;  {:test (fn []
;           (is (= (remove-opposites "dabAcCaCBAcCcaDA") "dabCBAcaDA")))}
;  [text]
;  (loop [current-text (map-indexed vector text)
;         index 0]
;    (if (= index (dec (count current-text)))
;      (str/join (map second current-text))
;      (if (= (str (second (nth current-text index))) (reverse-case (str (second (nth current-text (inc index))))))
;        (recur (drop-nth #{index (inc index)} current-text)
;               (if (= -1 (dec index))
;                 0
;                 (dec index)))
;        (recur current-text (inc index))))))

