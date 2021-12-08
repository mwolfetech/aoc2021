(ns aoc2021.day5
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def sample-data (slurp "./data/d5s.in"))
(def full-data (slurp "./data/d5.in"))


(defn parse [input]
 (let [pattern #"(\d+),(\d+) -> (\d+),(\d+)"]
   (for [s input
         :let [matcher (re-matcher pattern s)
               matches ((comp rest re-find) matcher)
               line-name s]]
     (zipmap [:line-name :x1 :y1 :x2 :y2] (conj (map read-string matches) line-name)))))


(defn input [data]
  (-> data 
      str/split-lines
      parse))

(defn get-line-slope [x1 y1 x2 y2]
  (/ (- y2 y1)(- x2 x1)))

(defn get-line-type [x1 y1 x2 y2]
  (cond (= x1 x2) "vertical"
        (= y1 y2) "horizontal"
        :else "diagonal"))


(defn get-line [{x1 :x1 y1 :y1 x2 :x2 y2 :y2 line-name :line-name}]
  (let [x-dir (if (neg? (- x2 x1)) -1 1)
        y-dir (if (neg? (- y2 y1)) -1 1)
        line-type (get-line-type x1 y1 x2 y2)
        x-range (range x1 (+ x2 x-dir) x-dir)
        y-range (range y1 (+ y2 y-dir) y-dir)]
    {:line-name line-name
     :values (if (= "diagonal" line-type) (map #(conj [%1] %2) x-range y-range)
                 (for [x x-range y y-range ] [x y]))
     :type (get-line-type x1 y1 x2 y2)}))


(defn calculate [lines]
  (let [allpoints (->> lines
                       (map :values)
                       (apply concat)
                       sort)]
    (->> (frequencies allpoints)
         (map val)
         (remove #{1})
         (count))))


(defn part1 [lines]
  (calculate (remove #(= "diagonal" (:type %)) lines)))

(defn part2 [lines]
  (calculate lines))

(defn -main [& args]
  (let [lines (->> (input full-data)
                   (map get-line))]
    [(part1 lines) (part2 lines)]))

 
