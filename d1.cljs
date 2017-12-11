(ns d1.core
  (:require [cljs.test :refer-macros [deftest is run-tests]]
            [planck.core :refer [slurp]]
            [planck.io :refer [file]]))

;; IMPLEMENTATION

(defn part1
  "Is the next one the same?"
  [_ coll]
  (let [[x y] coll] (= x y)))

(defn part2
  "Is the halfway around one the same?"
  [full coll]
  (= (first coll) (nth coll (quot (count full) 2))))

(defn sum-if
  "Sum of all numbers that match their buddy according to pred"
  [input pred]
  (loop [[x & xs :as coll] input acc 0]
    (cond
      (empty? coll) acc
      (pred input (concat coll input)) (recur xs (+ x acc))
      :else (recur xs acc))))

;; TESTS

(deftest part1-sample
  (is (= (sum-if [1 1 2 2] part1) 3))
  (is (= (sum-if [1 1 1 1] part1) 4))
  (is (= (sum-if [1 2 3 4] part1) 0))
  (is (= (sum-if [9 1 2 1 2 1 2 9] part1) 9)))

(deftest part2-sample
  (is (= (sum-if [1 2 1 2] part2) 6))
  (is (= (sum-if [1 2 2 1] part2) 0))
  (is (= (sum-if [1 2 3 4 2 5] part2) 4))
  (is (= (sum-if [1 2 3 1 2 3] part2) 12))
  (is (= (sum-if [1 2 1 3 1 4 1 5] part2) 4)))

;; RUN

(defn -main []
  (run-tests)
  (let [p (->> "d1.txt"
               (file)
               (slurp)
               (map js/parseInt)
               (remove js/isNaN))]
    (println (str "Part 1 output: " (sum-if p part1) "\n"
                  "Part 2 output: " (sum-if p part2) "\n"))))

(set! *main-cli-fn* -main)
