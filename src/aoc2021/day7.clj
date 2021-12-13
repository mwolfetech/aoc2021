(ns aoc2021.day7
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def sample-data (slurp "./data/d7s.in"))
(def full-data (slurp "./data/d7.in"))

(defn input
  [data]
  (->> data 
       str/split-lines
       first
       (#(edn/read-string (str"[" % "]")))))

;; originally used in part1 before brute force calculate below
(defn median [data]
  (let [sorted (sort data)
        sz (count sorted)]
    (if (= (mod sz 2) 0)
      (nth sorted (/ sz 2))
      (/  (+ (nth sorted (int (/ (dec sz) 2)))
             (nth  sorted(int (/ sz 2))))
          2))))

(defn cost [[x1 x2]]
  (- x2 x1))

(defn cost2 [point]
  (reduce (fn [res i]
            (+ res i))
          0
          (range 0 (inc (cost point)))))

(defn calculate [data f]
  (let [in (input data)
        full-range (range (apply min in) (inc (apply max in)))
        cost-map (zipmap full-range (repeat 0))]
    (->> (reduce  (fn [res v]
                    (let [calcs
                          (into {} (for [x full-range]
                                     [x (f (sort [x v]))]))]
                      (merge-with + res calcs)))
                  cost-map
                  in)
         (apply min-key second)
         (second))))

(defn part1
  [data]
  (calculate data cost))

(defn part2
  [data]
  (calculate data cost2))

(defn -main
  [& args]
  [(part1 full-data)
   (part2 full-data)])
