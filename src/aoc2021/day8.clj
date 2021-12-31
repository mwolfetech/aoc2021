(ns aoc2021.day8
  (:require [clojure.string :as str]
             [clojure.math.combinatorics :as combo]))

(def sample-data (slurp "./data/d8s.in"))
(def full-data (slurp "./data/d8.in"))

;;             a b c d e f g
(def display [[1 1 1 0 1 1 1]   ;; 0
              [0 0 1 0 0 1 0]   ;; 1
              [1 0 1 1 1 0 1]   ;; 2
              [1 0 1 1 0 1 1]   ;; 3
              [0 1 1 1 0 1 0]   ;; 4
              [1 1 0 1 0 1 1]   ;; 5
              [1 1 0 1 1 1 1]   ;; 6
              [1 0 1 0 0 1 0]   ;; 7
              [1 1 1 1 1 1 1]   ;; 8
              [1 1 1 1 0 1 1]]) ;; 9

(def combos (combo/permutations #{\a \b \c \d \e \f \g}))

(defn parse [input]
  (let [pattern #"(\w+) (\w+) (\w+) (\w+) (\w+) (\w+) (\w+) (\w+) (\w+) (\w+) \| (\w+) (\w+) (\w+) (\w+)"]
    (map-indexed (fn [i s]
                   (let [matcher (re-matcher pattern s)
                         matches ((comp rest re-find) matcher)
                         signals (vec (sort-by count (take 10 matches)))
                         digits (vec (drop 10 matches))
                         ]
                     {:signals signals
                      :digits digits
                      :lineno (inc i)})) input)))

(defn to-bit
  [s combo]
  (let [set (into #{} s)]
    (vec (map #(if (set %) 1 0) combo))))

(defn test-wire-config
  [s combo]
  (let [bit (to-bit s combo)]
    (some #{bit} display)))

(defn get-wire-config
  [signals]
  (first
    (loop [combos combos]
      (let [c (first combos)]
        (if (= (count (take-while #(test-wire-config % c) signals)) 10)
          c
          (recur (rest combos)))))))


(defn get-output [wire-config digits]
  (for [d digits]
    (->> (to-bit d wire-config)
         (.indexOf display)
         str)))

(defn decode [input]
  (let [wire-config (get-wire-config (:signals input))]
    (->> (get-output wire-config (:digits input))
         str/join
         Integer/parseInt)))

(defn part2
  [input]
  (reduce +
          (map decode input)))

(defn part1
  [input]
  (->> input
       (map #(-> %
                 (select-keys [:digits])
                 vals))
       flatten
       (map count)
       (filter #{2 4 3 7})
       count))

(defn input [data]
  (-> data
      str/split-lines
      parse))

(defn -main
  [& args]
  (let [input (input full-data)]
    [(part1 input)
     (part2 input)]))
