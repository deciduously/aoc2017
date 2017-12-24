(ns d7.core
  (:require [cljs.test :refer-macros [deftest is run-tests]]
            [clojure.string :refer [split]]
            [planck.core :refer [slurp]]))

;; IMPLEMENTATION

;TODO named groups?  re-seq?
(defn parse-line
  "Parse a string line into a hashmap of attributes"
  [l]
  {:name (re-find #"^\w+" l)
   :weight (-> (re-find #"\((\d+)\)" l) second js/parseInt)
   :children (let [match (second (re-find #"> (.+)$" l))] (if ((not nil?) match) (split match ", ")))})

(defn parse-input
  "Parse string input into list of hashmaps"
  [s]
  (->> (split s "\n")
       (map parse-line)))

; INCOMPLETE - just finds the root node, isn't actually pulling the full maps into the structure yet
(defn make-tree
  "Organizes parsed input into tree"
  [ps]
  (cond
    (map? ps) ps
    :otherwise (let [parents (filter #(string? (first (:children %))) ps)
                     matches (mapcat :children parents)
                     new-parents (map #(assoc % :children (filter (fn [x] (some #{(:name x)} (:children %))) ps)) parents)]
                    (first (filter #(not (some #{(:name %)} matches)) new-parents)))))

(defn bottom
  "Grab the name of the root node of the tree"
  [s]
  (:name (make-tree s)))

;; TESTS

(deftest test1
  (let [sample (slurp "d7-sample.txt")]
    (is (= (bottom (parse-input sample)) "tknk"))))

;; RUN

(defn -main []
  (run-tests)
  (let [puzzle (slurp "d7.txt")]
    (println (str "Part 1 output: " (bottom (parse-input puzzle))))))

(set! *main-cli-fn* -main)
