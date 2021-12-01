(ns aoc2021.day2
  (:require [clojure.string :as str]))


(def sample-data (slurp "./data/d2s.in"))

(def full-data (slurp "./data/d2.in"))


;; part1
(defn input [s]
  (->> s
       str/split-lines
       (map #(str/split % #" "))
       (map (fn [[a b]] {a (Integer/parseInt b)}))))

(defn calculate [m]
  (* (- (get m "down")(get m "up"))
     (get m "forward" )))

(defn part1 [data]
  (->> data
        (reduce (fn [res m]
                  (merge-with + res m)) {})
        calculate))


;; part2
(defn up [m v]  (merge-with - m {"aim" v}))

(defn down [m v] (merge-with + m {"aim" v}))

(defn forward [m v]
  (as-> m res
    (merge-with + res {"position" v})
    (merge-with + res {"depth" (* (get res "aim") v)})))

(defn calculate2 [m]
  (* (get m "position") (get m "depth")))

(defn part2 [data]
  (->> data
       (map vec)
       (reduce (fn [res [[k v]]]
                    (cond-> res
                      (= k "down") (down v)
                      (= k "up") (up v)
                      (= k "forward") (forward v))) {"aim" 0 "position" 0 "depth" 0})
       calculate2))


(defn -main
  [& args]
  (let [data (input full-data)]
    [(part1 data)
     (part2 data)
     ]))
