(ns aoc2018.day-four-test
  (:require [aoc2018.day-four :refer :all]
            [clojure.test :refer :all]))

(deftest test-to-lines
  (testing "to-lines splits, trims, and drops blank lines"
    (is (=
          (to-lines "
  line1

line2
     ")
          '("line1" "line2")))))

(deftest test-to-record
  (is (=
        (to-record "[1518-05-12 00:46] wakes up")
        {:datetime "1518-05-12 00:46" :message "wakes up"})))

(deftest test-guard?
  (is (guard? "Guard #1 arrives"))
  (is (boolean? (guard? "Guard #1 arrives")))
  (is (not (guard? "falls asleep"))))

(deftest test-guard-number
  (is (= (guard-number "Guard #1 arrives") "1"))
  (is (nil? (guard-number "not a guard"))))
