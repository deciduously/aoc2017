(ns d2.core
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [clojure.string :refer [split]]
            [planck.core :refer [slurp]]))

;; IMPLEMENTATION

(defn checksum
  "Adds up result of f for each row"
  [s f]
  (->> (split s "\n")
       (map #(split % "\t"))
       (map #(let [xs (map js/parseInt %)] (f xs)))
       (reduce +)))

(defn part1
  "Difference between the min and the max"
  [xs]
  (- (reduce max xs) (reduce min xs)))

(defn part2
  "Only whole number quotient"
  [xs]
  (->> (for [x xs y xs :when (not (= x y))] [x y])
       (filter #(= 0 (mod (first %) (second %))))
       (map #(/ (first %) (second %)))
       (reduce +)))

;; TESTS

(def sample1 "5\t1\t9\t5\n7\t5\t3\t\t\n2\t4\t6\t8\n")
(def sample2 "5\t9\t2\t8\n9\t4\t7\t3\n3\t8\t6\t5\n")

(deftest checksums
  (is (= (checksum sample1 part1) 18))
  (is (= (checksum sample2 part2) 9)))

;; RUN

(defn -main []
  (run-tests)
  (let [puzzle (slurp "d2.txt")]
    (println (str "Part 1 output: " (checksum puzzle part1)))
    (println (str "Part 1 output: " (checksum puzzle part2)))))

(set! *main-cli-fn* -main)
