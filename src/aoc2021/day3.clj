(ns aoc2021.day3
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def sample-data (slurp "./data/d3s.in"))
(def full-data (slurp "./data/d3.in"))

(defn int-vector-> [binarystring]
  (as-> binarystring s
    (interpose " " s)
    (apply str s)
    (str "[" s "]")
    (edn/read-string s)))

(defn transpose [vs]
  (apply map vector vs))

(defn input [s]
  (-> s
      str/split-lines))

(defn average [c] 
  (/ (reduce + c) (count c)))

(def gamma {false 0 true 1})
(def epsilon {false 1 true 0})

(defn twist-it [data]
  (->> data
       (map int-vector->)
       (transpose)))

(defn smash-it [type v]
  (->> (map (comp type #(< 1/2 %) average) v)
       (apply str)))

(defn binary-vec-to-dec [type v]
  (->> (smash-it type v)
       (#(BigInteger. % 2))))

(defn calculate [v]
  (* (binary-vec-to-dec gamma v)
     (binary-vec-to-dec epsilon v)))

(defn part1 [data]
  (->> data
   (input)
   (twist-it)
   (calculate)))

(defn do-calculation [type input]
  (->> input
       (twist-it)
       (smash-it type)))

(defn grep [pos chr strs]
  (->> strs
      (filter #(= chr (nth % pos)))))


;; this is Aoc hackery - needs refactored now that it works 
(defn search [type type-value vs]
  (first (loop  [n 0
                 chr (first type-value)
                 candidates vs
                 selected (grep 0 chr vs)
                 ]
           (let [half-sz (/ (count candidates) 2)
                 n++ (inc n)
                 new-type-value (if (seq selected) (do-calculation type selected) selected)
                 new-chr (get new-type-value n++)
                 preferred-char (char (+ 48 (get type true)))]
             (cond (= 1 (count candidates)) candidates
                   (and  (= half-sz (count selected)) (not= chr preferred-char))
                   (if (= 1 (count selected))
                     (grep n preferred-char candidates)
                     (recur n preferred-char candidates (grep n preferred-char candidates)))
                   :else (recur n++ new-chr selected (grep n++ new-chr selected)))))))

(defn part2 [data]
  (let [vs (input data)]
    (->> (for [type [aoc2021.day3/gamma
                     aoc2021.day3/epsilon]]
           (let [ type-value (smash-it type (twist-it vs))]
             (search type type-value vs)))
         (flatten)
         (map #(BigInteger. % 2))
         (reduce *))))

(defn -main [& args]
  [(part1 full-data)
   (part2 full-data)])



