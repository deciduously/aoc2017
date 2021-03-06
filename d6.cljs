(ns d6.core
  (:require [cljs.test :refer-macros [deftest is run-tests]]
            [clojure.string :refer [split]]
            [planck.core :refer [slurp]]))

;; IMPLEMENTATION

(def sample "0\t2\t7\t0\n")

(defn parse-input
  "Get list of numbers from string input"
  [s]
  (->> (split s "\t") (map js/parseInt)))

(defn redist
  "Redistribute"
  [blocks]
  (let [size (count blocks)
        max-val (reduce max blocks)
        max-idx (.indexOf blocks max-val)
        next-idx (inc max-idx)
        leftovers (map #(mod % size) (range next-idx (+ next-idx (rem max-val size))))] ; indexes of remainders to increment
    (->> (flatten (cons (cons (take max-idx blocks) '(0)) (drop next-idx blocks))) ; set max to 0 - pattern feels dumb and clunky - split-at would be gross too.
         (map #(+ (quot max-val size) %)) ; add the even amount across the board
         (map-indexed #(if (some #{%} leftovers) (inc %2) (identity %2)))))) ; and increment the proper amount for the remainder

; TODO see if you can find a way without leaning on loop/recur - some sort of take-while on an iterate redist would be nice
(defn find-dup
  "Redistributes until we find a duplicate, returns how many iterations it took"
  [blocks]
  (loop [coll (list blocks) acc 0]
    (let [curr (first coll)]
      (if (some #{curr} (rest coll))
        {:result acc :repeated curr}
        (recur (cons (redist curr) coll) (inc acc))))))

;; TESTS

(deftest sample1
  (is (= (:result (find-dup (parse-input sample))) 5)))

;; RUN

(defn -main []
  (run-tests)
  (let [puzzle (parse-input (slurp "d6.txt"))
        part1 (find-dup puzzle)] ; takes about 2 minutes on my machine
    (println (str "Part 1: " (:result part1)))
    (println (str "Part 2: " (:result (find-dup (:repeated part1)))))))

(set! *main-cli-fn* -main)
