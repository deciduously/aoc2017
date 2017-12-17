(ns d4.core
  (:require [cljs.test :refer-macros [deftest is run-tests]]
            [clojure.string :refer [split]]
            [planck.core :refer [slurp]]))

;; IMPLEMENTATION

(defn valid?
  "True if contains no duplicate words"
  [s]
  (let [words (split s " ")]
    (->> (map #(->> words (filter #{%}) count) words)
         (filter #(> % 1))
         (empty?))))

(defn part1
  "Given string, returns how many lines are valid"
  [s]
  (->> (split s "\n")
       (filter valid?)
       (count)))

;; TESTS

(deftest test1
  (is (valid? "aa bb cc dd ee"))
  (is (not (valid? "aa bb cc dd aa")))
  (is (valid? "aa bb cc dd aaa")))

;; RUN

(defn -main []
  (let [puzzle (slurp "d4.txt")]
    (run-tests)
    (println (str "Part 1 output: " (part1 puzzle)))))

(set! *main-cli-fn* -main)
