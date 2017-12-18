(ns d5.core
  (:require [cljs.test :refer-macros [deftest is run-tests]]
            [clojure.string :refer [split]]
            [planck.core :refer [slurp]]))

;; IMPLEMENTATION

(def sample "0\n3\n0\n1\n-3\n")

(defn jumps
  "Get number of steps required to reach exit"
  [s]
  (loop [curr (->> (split s "\n") (map js/parseInt)) idx 0 acc 0]
    (println (str curr " idx: " idx " acc: " acc)) ; for debugging - left in for something to watch
    (if (or (< idx 0) (>= idx (count curr)))
      acc
      (let [next-idx (+ (nth curr idx) idx)
            update-curr (flatten (cons (cons (take idx curr) (list (inc (nth curr idx)))) (drop (inc idx) curr)))]
        (recur update-curr next-idx (inc acc))))))

;; TESTS

(deftest test1
  (is (= (jumps sample) 5)))

;; RUN

(defn -main []
  (let [puzzle (slurp "d5.txt")]
    (run-tests)
    (println (str "Part 1 output: " (jumps puzzle))))) ; NOTE takes over a half hour on my machine

(set! *main-cli-fn* -main)
