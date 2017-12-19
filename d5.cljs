(ns d5.core
  (:require [cljs.test :refer-macros [deftest is run-tests]] ; Just grab some utilities
            [clojure.string :refer [split]]
            [planck.core :refer [slurp]]))

;; IMPLEMENTATION - TODO benchmark against mutating an atom, I'm not sure but keeping everything nice and pure and immutable might be slowing this down

(def sample "0\n3\n0\n1\n-3\n")


(defn part1
  "Always increment"
  [s]
  (inc s))

(defn part2
  "Decrement if three or more, otherwise increment"
  [s]
  (if (>= s 3) (dec s) (inc s)))

(defn jumps
  "Get number of steps required to reach exit, using f to determine step change"
  [f s]
  (loop [curr (->> (split s "\n") (map js/parseInt)) idx 0 acc 0]
    ; (println (str curr " idx: " idx " acc: " acc)) ; for debugging only
    (if (or (< idx 0) (>= idx (count curr)))
      acc
      (let [offset (nth curr idx)]
        (recur (flatten (cons (cons (take idx curr) (list (f offset))) (drop (inc idx) curr))) (+ offset idx) (inc acc))))))

;; TESTS

(deftest tests
  (is (= (jumps part1 sample) 5))
  (is (= (jumps part2 sample) 10)))

;; RUN

(defn -main []
  (let [puzzle (slurp "d5.txt")]
    (run-tests)
    (println (str "Part 1 output: " (jumps part1 puzzle))) ; NOTE - these take a very long time.  To check if they're working, uncomment line 24.
    (println (str "Part 2 output: " (jumps part2 puzzle))))) ; Run at your discretion - to submit, I called each separately from the REPL.

(set! *main-cli-fn* -main)
