(ns aoc2021.day4
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def sample-data (slurp "./data/d4s.in"))
(def full-data (slurp "./data/d4.in"))

(defn int-vector-> [binarystring]
  (as-> binarystring s
    (interpose " " s)
    (apply str s)
    (str "[" s "]")
    (edn/read-string s)))

(defn transpose [vs]
  (apply map vector vs))


(defn mark-square [square card]
  (let [[left right] (split-with (complement #{[square 0]}) card)]
    (vec (concat left [[square 1]] (rest right)))))

(defn input [s]
  (let [data (->> s
                  (str/split-lines)
                  (remove str/blank?)
                  (map #(str "[" % "]"))
                  (map edn/read-string))
        [drawbox & cards] data]
    {:drawbox drawbox
     :cards (->> (interleave (flatten cards) (repeat 0))
                 (partition 2)
                 (map vec)
                 (partition 25)
                 (map vec))}))


(defn check-card [state card card-sequences]
  (reduce (fn [res candidate-seq]
            (if (= (count (take-while #(= (last %) 1) candidate-seq)) 5)
              (merge res {:cards (remove #{card} (:cards state))
                          :winning-cards (conj (:winning-cards res) card)
                          :win true})
              res)) state card-sequences))

(defn calculate-win [state]
  (reduce (fn [new-state card]
            (let [c (partition 5 card)
                  t (transpose c)
                  m (concat c t)]
              (check-card new-state card m)))
          state
          (:cards state)))


(defn mark-cards [draw cards]
  (for [card cards]
              (if (some #{[draw 0]} card)
                (mark-square draw card)
                card)))

(defn play [initial-state]
  (reduce (fn [state draw]
            (let [cards (:cards state)
                  new-state (-> state
                                (merge
                                 {:drawbox (rest (:drawbox state))
                                  :cards (mark-cards draw cards)})
                                calculate-win)]
              (if (:win new-state)
                (-> new-state
                    (assoc :winning-draws (conj (:winning-draws new-state) draw) :win false))
                new-state)))
          (merge initial-state
                 {:card-count (count (:cards initial-state))
                  :win false
                  :winning-cards []
                  :winning-draws []})
          (:drawbox initial-state)))


(defn calculate [f data]
  (let [result (-> (input data)
                   play)
        unmarked (map first (filter #(= (last %) 0) (f (:winning-cards result))))]
    (* (reduce + unmarked) (first (:winning-draws result)) )))

(defn -main [& args]
  [(calculate first full-data) ;;part1
   (calculate last full-data) ;; part2
   ])
