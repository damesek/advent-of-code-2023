(ns  day01
  (:require [clojure.string :refer [split-lines join]]))


(def example
  ["1abc2"
   "pqr3stu8vwx"
   "a1b2c3d4e5"
   "treb7uchet"])

(def input-data
  (->> "input.txt"
       slurp
       split-lines))

(defn get-number [^String s]
  (let [numbers (re-seq #"\d" s)
        _ (println "numbers " numbers)
         get-two-digit-number
           (fn [numbers]
             ((comp #(Integer/parseInt %)
                          #(str (first %) (last %))) numbers))]
      ((fnil get-two-digit-number "0") numbers)))


(defn get-sum-of-numbers [strings]
  (reduce + (mapv get-number strings)))


;; part1 result
;;(get-sum-of-numbers input-data)


(def example-part2
  ["two1nine"
   "eightwothree"
   "abcone2threexyz"
   "xtwone3four"
   "4nineeightseven2"
   "zoneight234"
   "7pqrstsixteen"])

(def map-of-strings
  {"1" 1
   "2" 2
   "3" 3
   "4" 4
   "5" 5
   "6" 6
   "7" 7
   "8" 8
   "9" 9
   "one" 1
   "two" 2
   "three" 3
   "four" 4
   "five" 5
   "six" 6
   "seven" 7
   "eight" 8
   "nine" 9})



(defn generate-regex-pattern
  "Generates a regex pattern from the keys of a map."
  [m]
  (->> (keys m)
         (join "|")))


(def new-pattern
  "get back overlaps too in order"
  (re-pattern
   (str "(?=(" (generate-regex-pattern map-of-strings) "))")))


(defn extract-number-part2 [s]
  (let [numbers (keep (fn [[_ m]]
                            (get map-of-strings m))
                            (re-seq new-pattern s))
        numbers-handle-nil (if (empty? numbers) 0 numbers)
        get-two-digit (->> (str (first numbers-handle-nil)
                                           (last numbers-handle-nil))
                                    Integer/parseInt)]
    get-two-digit))


(defn get-part2-result []
  (reduce + (map extract-number-part2 input-data)))
