(ns d4.core
  (:require [cljs.test :refer-macros [deftest is run-tests]]
            [clojure.string :refer [split]]
            [planck.core :refer [slurp]]))

;; IMPLEMENTATION

(defn part1
  "Get how many times w occurs in ws"
  [w ws]
  (->> ws (filter #{w}) count))

(defn part2
  "Get how many anagrams of w occur in ws"
  [w ws]
  (->> ws (filter #(= (sort w) (sort %))) count))

(defn valid?
  "True if contains no duplicate words"
  [f s]
  (let [words (split s " ")]
    (->> (map #(f % words) words)
         (filter #(> % 1))
         (empty?))))

(defn count-valid
  "Given string, returns how many lines are valid"
  [f s]
  (->> (split s "\n")
       (filter #(valid? f %))
       (count)))

;; TESTS

(deftest sample1
  (is (valid? part1 "aa bb cc dd ee"))
  (is (not (valid? part1 "aa bb cc dd aa")))
  (is (valid? part1 "aa bb cc dd aaa")))

(deftest sample2
  (is (valid? part2 "abcde fghij"))
  (is (not (valid? part2 "abcde xyz ecdab")))
  (is (valid? part2 "a ab abc abd abf abj"))
  (is (valid? part2 "iiii oiii ooii oooi oooo"))
  (is (not (valid? part2 "oiii ioii iioi iiio"))))

;; RUN

(defn -main []
  (let [puzzle (slurp "d4.txt")]
    (run-tests)
    (println (str "Part 1 output: " (count-valid part1 puzzle)))
    (println (str "Part 2 output: " (count-valid part2 puzzle)))))

(set! *main-cli-fn* -main)
