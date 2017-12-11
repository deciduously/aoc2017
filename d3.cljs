#!/usr/bin/env planck
(ns d3.core
  (:require [cljs.test :refer-macros [deftest is run-tests]]
            [planck.core :refer [slurp]]
            [planck.io :refer [file]]))

;; IMPLEMENTATION

(def part1 (->> (iterate inc 0) (map #(inc (* 2 %))) (map #(.pow js/Math % 2)))) ;[3x3,5x5,7x7,...]

(defn layer
  "Returns how many layers out x is in coll"
  [fill n]
  (loop [[x & xs] fill acc 0]
    (if (<= n x) acc (recur xs (inc acc)))))

(defn data-carry
  "Manhattan distance of x to center of spiral grid"
  [fill x]
  (if (< 1 x)
    (let [l (layer fill x)]
      (->> (range (- (nth fill l) l) (inc (nth fill (dec l))) (- (* 2 l))) ;; midpoints of each edge of the layer
           (map #(.abs js/Math (- x %)))
           (reduce min)
           (+ l))) ;; final total is distance from closest midpoint in the row + total rows
    0))

;; TESTS

(deftest sample1
  (is (= (data-carry part1 1) 0))
  (is (= (data-carry part1 12) 3))
  (is (= (data-carry part1 23) 2))
  (is (= (data-carry part1 1024) 31)))

;; RUN

(defn -main [& args]
  (run-tests)
  (println (str "Part 1 output: " (data-carry part1 312051))))

(set! *main-cli-fn* -main)


