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

;;This is incredibly slooow - I think you can use the fact that you're only adding the inside edge

(defn cell [n x y b] {:n n :x x :y y :b b})

(defn turn-left
  "get whats left from b"
  [b]
  (cond
    (= b "right") "up"
    (= b "left") "down"
    (= b "up") "left"
    (= b "down") "right"))

(defn coords
  "next cell"
  [c]
  (let [{:keys [n x y b]} c]
    (cond
      (= b "right") (cell n (inc x) y b)
      (= b "left") (cell n (dec x) y b)
      (= b "up") (cell n x (inc y) b)
      (= b "down") (cell n x (dec y) b))))

(defn next-cell
  "Returns the next cell, given the bearing"
  [c]
  (let [l (layer (:n c))]
    (if (= l 0)
      (cell 2 1 0 "up") ; the below needs a layer higher than 0, so explicitly define that case
      (let [{:keys [n x y b]} c
            turns (cons (+ 2 (nth layer-sizes (dec l))) ; we turn at the second each time
                        (rest (range (nth layer-sizes l)
                                     (if (> l 1) (nth layer-sizes (dec l)) 1) ; gross - deal with layer 1 better
                                     (- (* 2 l)))))
            b' (if (empty? (filter #(= % (inc n)) turns)) b (turn-left b))]
        (update-in (coords (cell (inc n) x y b)) [:b] #(identity b'))))))

(defn grid
  "build the grid up to n"
  [n]
  (take n (iterate #(conj % (next-cell %)) (cell 1 0 0 "right"))))

;;brain flash bafore bed - look at the neighbors function instead.  you're adding just the inside rings of each, you might be able to do this easier
(defn neighbor-sum
  "Sums the neighbors of c"
  [c]
  c)

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

