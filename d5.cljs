;; Hi Jeremy!  I tried to be explicit enough that you don't need to read the code, but if you weren't aware any Lisp is built of s-expressions.
;; What this means for you is that a function call looks like: (func args) - the first position in a paren is (almost) always a function or macro
;; Mathematical operations are normal function calls, always prefix: (+ 1 2) yields 3, (- 1 2) yields -1
;; To get a regular list, i.e., don't treat the first position as a function but merely data, you single quote: '(0 1 2) is a list with three elements
;; Also, commas are whitespace, so most omit them.  [1,2,3] is equivalent to [1 2 3]
;; You shouldn't need to care about the shape of the brackets (list vs. vector) to think about this problem, there are differences but not big conceptual ones
;; Let me know if I was not clear anywhere

(ns d5.core
  (:require [cljs.test :refer-macros [deftest is run-tests]] ; Just grab some utilities
            [clojure.string :refer [split]]
            [planck.core :refer [slurp]]))

;; IMPLEMENTATION - TODO benchmark against mutating an atom, I'm not sure but keeping everything nice and pure and immutable might be slowing this down

(def sample "0\n3\n0\n1\n-3\n") ; turns in to '(0 3 0 1 -3)


(defn part1
  "Always increment" ; (defn) takes an optional docstring before the arguments, this is not semantic
  [s]
  (inc s)) ; just adds one to whatever is passed in

(defn part2
  "Decrement if three or more, otherwise increment"
  [s]
  (if (>= s 3) (dec s) (inc s))) ; If works differently than C-style, it always evaluates to a value.  It takes a predicate, a value if true, and a value if false

(defn jumps ; Entry point
  "Get number of steps required to reach exit, using f to determine step change"
  [f s] ; Takes two arguments - the function to update with (one of the two above) and the list of steps.  It expects a string formatted like sample above
  ; Loop/recur is the construct I use.  It initializes the loop parameters, and then when it reaches the recur keyword it runs again from the loop,
  ; passing in the new values for these parameters.  This sort of performs tail-call optimization by ensuring you're recurring on a tail call and handling it well
  ; Clojure otherwise does not perform that optimization on a naked recursion (which is a point of frustration for me)
  ; but this automatically tosses the previous iteration, which otherwise is my first instinct for optimizing things like this
  ; My loop defines `curr` as the result of splitting the steps on newlines, and running them through the host function parseInt to get ints from strings
  ; I start with the current instruction idx at 0, and an accumulator at 0 to track iterations and eventually return
  (loop [curr (->> (split s "\n") (map js/parseInt)) idx 0 acc 0]
    ; (println (str curr " idx: " idx " acc: " acc)) ; for debugging only, probably slows things down - just prints out the current values stored
    (if (or (< idx 0) (>= idx (count curr))) ; Each time we run the loop, check if our index is out of bounds
      acc ; Value if true - if index is out of bounds, we're done - return accumulator
      (let [offset (nth curr idx)] ; Otherwise, grab the value where your pointer is
        ; and recur.  The big flatten statement essentially reads:
        ; "Take the list up to where I am, mash it together with the result of the function I'm calling, and attach the end list after my point to it"
        ; As a side note, I'm almost positive there's a less gross way to express this - there is for key-value hashmaps but I cant find it for plain lists
        ; I don't believe the ugliness is incurring a performance penalty, though, just a readability one
        ; This is how you avoid mutation - instead of changing a value, I'm creating a whole new list with parts from the old one.  The old one is not touched
        ; (+ offset idx) puts your pointer on the next instruction (or out of the list)
        ; And we add one to the accumulator to run again
        (recur (flatten (cons (cons (take idx curr) (list (f offset))) (drop (inc idx) curr))) (+ offset idx) (inc acc))))))

;; You probably don't need to look anywhere below this, just calls the above functions with the proper inputs

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
