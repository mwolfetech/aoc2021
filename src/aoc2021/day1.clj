(ns aoc2021.day1
  (:require [clojure.string :as str]))


(def sample-data (slurp "./data/d1s.in"))

(def full-data (slurp "./data/d1.in"))

(defn input [s]
  (->> s
      str/split-lines
      (map #(Integer/parseInt %))))

(defn part1 [data]
  (->> (map < data (rest data))
      (remove false?)
      count))


(defn part2 [data]
  (->> data
       (partition 3 1)
       (map #(reduce + %))
       (part1)))

(defn -main
  [& args]
  (let [data (input full-data)]
    [(part1 data)
     (part2 data)]))
