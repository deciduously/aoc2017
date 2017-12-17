#!/usr/bin/env planck
(ns d3.core
  (:require [cljs.test :refer-macros [deftest is run-tests]]
            [planck.core :refer [slurp]]
            [planck.io :refer [file]]))

;; IMPLEMENTATION

;; PART 1

;; [3x3,5x5,7x7,...]
(def layer-sizes (->> (iterate inc 0)
                      (map #(inc (* 2 %)))
                      (map #(.pow js/Math % 2))))

(defn layer
  "Returns how many layers out x is in coll"
  [n]
  (loop [[x & xs] layer-sizes acc 0]
    (if (<= n x) acc (recur xs (inc acc)))))

(defn data-carry
  "Manhattan distance of x to center of spiral grid"
  [x]
  (if (< 1 x)
    (let [l (layer x)]
      (->> (range (- (nth layer-sizes l) l) (inc (nth layer-sizes (dec l))) (- (* 2 l))) ;; midpoints of each edge of the layer
           (map #(.abs js/Math (- x %)))
           (reduce min)
           (+ l))) ;; final total is distance from closest midpoint in the row + total rows
    0))

;; PART 2

;; This is incredibly slow

; idx xcoord ycoord bearing value
(defn cell [n x y b v] {:n n :x x :y y :b b :v v})

(defn coords
  "next cell"
  [c]
  (let [b (:b c)]
    (cond
      (= b "right") (update-in c [:x] inc)
      (= b "left") (update-in c [:x] dec)
      (= b "up") (update-in c [:y] inc)
      (= b "down") (update-in c [:y] dec))))

;; you're adding just the inside rings of each, you might be able to do this easier
(comment (defn neighbor-sum)
  "Sums the neighbors of c in g"
  [c]
  (let [b (:b c)]
    (cond
      (= b "right") 1
      (= b "left") 2
      (= b "up") 3
      (= b "down") 4)))

(defn next-cell
  "Returns the next cell, given the bearing"
  [c]
  (let [n (:n c)
        l (layer n)]
    (if (= l 0)
      (cell 2 1 0 "up" 2) ; the below needs a layer higher than 0, so explicitly define that case
      (let [{:keys [x y b]} c
            last-layer-size (nth layer-sizes (dec l))
            turns (cons (+ 2 last-layer-size) ; we turn at the second each time
                        (rest (range (nth layer-sizes l)
                                     (if (> l 1) last-layer-size 1) ; gross - deal with layer 1 better
                                     (- (* 2 l)))))
            b' (if (empty? (filter #(= % (inc n)) turns)) b (cond
                                                              (= b "right") "up"
                                                              (= b "left") "down"
                                                              (= b "up") "left"
                                                              (= b "down") "right"))]
        (update-in (coords (cell (inc n) x y b (inc n))) [:b] #(identity b'))))))

(defn grid
  "build the grid up to n"
  [n]
  (take n (iterate #(conj % (next-cell %)) (cell 1 0 0 "right" 1))))

;; TODO look for pretty-print in commit history - or write it.  you need to get this into a grid by values to get the neighbors.
;; alternatively, filter through the grid you've got with an (or) to capture all around?

;; TESTS

(deftest sample1
  (is (= (data-carry 1) 0))
  (is (= (data-carry 12) 3))
  (is (= (data-carry 23) 2))
  (is (= (data-carry 1024) 31)))

;; RUN

(defn -main [& args]
  (let [puzzle 312051]
    (run-tests)
    (println (str "Part 1 output: " (data-carry puzzle)))))
;;(println (str "Part 2 output: " (accumulator part2 )))))

(set! *main-cli-fn* -main)
