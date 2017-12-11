#!/usr/bin/env planck
(ns d3.core
  (:require [cljs.test :refer-macros [deftest is run-tests]]
            [planck.core :refer [slurp]]
            [planck.io :refer [file]]))

;; IMPLEMENTATION

(def row-sizes (->> (iterate inc 0) (map #(inc (* 2 %))) (map #(.pow js/Math % 2)))) ;[3x3,5x5,7x7,...]

(defn layer
  "Returns how many layers out x is in coll"
  [n]
  (loop [[x & xs] row-sizes acc 0]
    (if (<= n x) acc (recur xs (inc acc)))))

(defn data-carry
  "Manhattan distance of x to center of spiral grid"
  [x]
  (if (< 1 x)
    (let [l (layer x)
          ;; centers contains the midpoint of each edge of the layer
          centers (range (- (nth row-sizes l) l) (inc (nth row-sizes (dec l))) (- (* 2 l)))
          offset (reduce min (map #(.abs js/Math (- x %)) centers))]
      (+ l offset))
    0))

;; TESTS

(deftest sample1
  (is (= (data-carry 1) 0))
  (is (= (data-carry 12) 3))
  (is (= (data-carry 23) 2))
  (is (= (data-carry 1024) 31)))

;; RUN

(defn -main [& args]
  (run-tests)
  (println (str "Part 1 output: " (data-carry 312051))))

(set! *main-cli-fn* -main)


