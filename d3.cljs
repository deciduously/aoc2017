(ns d3.core
  (:require [cljs.test :refer-macros [deftest is run-tests]]))

;; IMPLEMENTATION

;; [3x3,5x5,7x7,...]
(def layer-sizes (->> (iterate inc 0)
                      (map #(inc (* 2 %)))
                      (map #(.pow js/Math % 2))))

(defn layer
  "Returns how many layers out x is in coll"
  [n]
  (loop [[x & xs] layer-sizes acc 0]
    (if (<= n x) acc (recur xs (inc acc)))))

;; PART 1

(defn part1
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

(defn cell [idx xcoord ycoord bearing value] {:n idx :x xcoord :y ycoord :b bearing :v value})

(defn coords
  "Next cell in cells direction"
  [c]
  (let [b (:b c)]
    (cond
      (= b "right") (update-in c [:x] inc)
      (= b "left") (update-in c [:x] dec)
      (= b "up") (update-in c [:y] inc)
      (= b "down") (update-in c [:y] dec))))

(defn neighbor-sum
  "Sum the vs of the neighboring cells of the last cell in g"
  [g]
  (let [c (first g)
        b (:b (second g)) ; penultimate
        {:keys [x y]} c
        neighbors (cond
                    (= b "right") (filter #(or (and (= (:y %) (inc y)) (or (= (:x %) x) (= (:x %) (inc x)) (= (:x %) (dec x))))
                                               (and (= (:x %) (dec x)) (= (:y %) y))) g)
                    (= b "left") (filter #(or (and (= (:y %) (dec y)) (or (= (:x %) x) (= (:x %) (dec x)) (= (:x %) (inc x))))
                                              (and (= (:x %) (inc x)) (= (:y %) y))) g)
                    (= b "up") (filter #(or (and (= (:x %) (dec x)) (or (= (:y %) y) (= (:y %) (inc y)) (= (:y %) (dec y))))
                                            (and (= (:y %) (dec y)) (= (:x %) x))) g)
                    (= b "down") (filter #(or (and (= (:x %) (inc x)) (or (= (:y %) y) (= (:y %) (inc y)) (= (:y %) (dec y))))
                                              (and (= (:y %) (inc y))(= (:x %) x))) g))]
    (->> neighbors (map :v) (reduce +))))

(defn next-cell
  "Returns a new grid with next cell added"
  [g]
  (let [c (first g)
        n (:n c)
        l (layer (inc n))]
    (if (= l 0)
      (cons (cell 2 1 0 "up" 1) (list g)) ; the below needs a layer higher than 0, so explicitly define that case
      (let [{:keys [x y b v]} c
            last-layer-size (if (> l 1) (nth layer-sizes (dec l)) 1)
            turns (cons (+ 1 last-layer-size) (rest (range (nth layer-sizes l) last-layer-size (- (* 2 l))))) ; turn at lowest in layer, and each corner
            b' (if (empty? (filter #(= % (inc n)) turns)) b (cond
                                                              (= b "right") "up"
                                                              (= b "left") "down"
                                                              (= b "up") "left"
                                                              (= b "down") "right"))
            untotaled (cons (update-in (coords (cell (inc n) x y b v)) [:b] #(identity b')) g)] ; move the direction we were moving, THEN change bearing
        (cons (update-in (first untotaled) [:v] #(neighbor-sum untotaled)) (rest untotaled)))))) ; and finally store the neighbor sum in the value

(defn part2
  "build the grid until value exceeds n, return that value"
  [n]
  (->> (cell 1 0 0 "right" 1)
       (iterate next-cell)
       (take-while #(<= (:v (first %)) n))
       (last)
       (next-cell)
       (first)
       (:v)))

;; TESTS

(deftest sample1
  (is (= (part1 1) 0))
  (is (= (part1 12) 3))
  (is (= (part1 23) 2))
  (is (= (part1 1024) 31)))

;; RUN

(defn -main [& args]
  (let [puzzle 312051]
    (run-tests)
    (println (str "Part 1 output: " (part1 puzzle)))
    (println (str "Part 2 output: " (part2 puzzle)))))

(set! *main-cli-fn* -main)
