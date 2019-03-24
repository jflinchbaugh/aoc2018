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
       {:datetime "1518-05-12 00:46" :message "wakes up"}
       (to-record "[1518-05-12 00:46] wakes up"))))

(deftest test-guard?
  (is (guard? "Guard #1 arrives"))
  (is (boolean? (guard? "Guard #1 arrives")))
  (is (not (guard? "falls asleep"))))

(deftest test-guard-number
  (is (= "1" (guard-number "Guard #1 arrives")))
  (is (nil? (guard-number "not a guard"))))

(deftest test-date
  (is (= "1234" (date "1234 4443"))))

(deftest test-guard-by-date
  (is (=
       {"dt" "10"}
       (guard-by-date
        nil
        {:datetime "dt",
         :message "Guard #10 arrives"})))
  (is (=
       {"dt" "10", "existing" "11"}
       (guard-by-date
        {"existing" "11"}
        {:datetime "dt",
         :message "Guard #10 arrives"}))))
(deftest test-get-guard
  (let [m {"2019-01-01 01:00" "g", "2019-01-01 23:59" "h"}]
    (is
     (= "g"
        (get-guard m "2019-01-01 02:00")))
    (is
     (= "h"
        (get-guard m "2019-01-02 02:00")))
    (is
     (nil?
      (get-guard m "2019-01-01 00:00")))
    (is
     (nil?
      (get-guard nil "2019-01-01 00:00")))))
