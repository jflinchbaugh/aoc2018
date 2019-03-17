(ns aoc2018.day-four-test
  (:require [aoc2018.day-four :refer :all]
            [clojure.test :refer :all]))

(deftest test-to-lines
  (testing "to-lines splits"
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
