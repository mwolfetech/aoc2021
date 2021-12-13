(ns aoc2021.day6
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def sample-data (slurp "./data/d6s.in"))
(def full-data (slurp "./data/d6.in"))


(defn input [data]
  (->> data
      str/split-lines
      first
      (#(edn/read-string (str"[" % "]")))))


(defn do-cycle [m]
  (let [changeset
         (reduce-kv (fn [res cycle-group curr]
                     (if (= 0 cycle-group)
                       (merge-with + res {8 curr
                                   6 curr
                                   0 (- curr)})
                       (merge-with + res { (dec cycle-group) curr
                                     cycle-group (- curr)})))
                   {0 0 1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0}
m
                   )]
(into {} (remove (comp zero? val)(merge-with + m changeset)))))

(defn calculate [days input]
  (->>  (loop [m (reduce-kv #(assoc %1 %2 (bigint %3)) {} (frequencies input))
              n days]
         (if (= 0 n)
             m
         (recur (do-cycle m) (dec n))))
       (vals)
       (reduce +)))


(defn part1 [input]
  (calculate 80 input))

(defn part2 [input]
  (calculate 256 input))

(defn -main [& args]
  (let [input (input full-data)]
    [(part1 input)
     (part2 input)]))
