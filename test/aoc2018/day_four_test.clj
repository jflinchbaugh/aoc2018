(ns aoc2018.day-four-test
  (:require [aoc2018.day-four :refer :all]
            [clojure.test :refer :all]))

(deftest test-to-lines
  (testing "to-lines splits, trims, and drops blank lines"
    (is (=
         `("line1" "line2")
         (to-lines "
  line1

line2
     ")))))

(deftest test-to-record
  (is (=
       {:datetime "1518-05-12 00:46" :message "wakes up"}
       (to-record "[1518-05-12 00:46] wakes up"))))

(deftest test-guard?
  (is (guard? {:message "Guard #1 arrives"}))
  (is (boolean? (guard? {:message "Guard #1 arrives"})))
  (is (not (guard? {:message "falls asleep"}))))

(deftest test-guard-number
  (is (= "1" (guard-number {:message "Guard #1 arrives"})))
  (is (nil? (guard-number {:message "not a guard"}))))

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
        (get-guard m "2019-01-01 01:00")))
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

(deftest test-with-guard-by-date
  (is
    (= {:datetime "dt" :message "m" :guard "guard"}
      (with-guard-by-date #(if (= (:datetime %) "dt") "guard") {:datetime "dt" :message "m"})))
  (is
    (= {:datetime "dt" :message "m" :guard nil}
      (with-guard-by-date #(and % nil) {:datetime "dt" :message "m"}))))

(deftest test-minute
  (is
    (= 2 (minute {:datetime "date 01:02"})))
  (is
    (nil? (minute {:datetime "bad"}))))

(deftest test-parse-int
  (is (= 0 (parse-int "0")))
  (is (nil? (parse-int "bad")))
  (is (nil? (parse-int nil)))
  )

(deftest test-durations
  (is (= [5 5] (durations [0 5 10 15]))))

(deftest test-total-minutes
  (is (= 10 (total-minutes
              [{:minute 0}
               {:minute 5}
               {:minute 10}
               {:minute 15}]))))

(deftest test-map-value
  (is (= {:a 2 :b 3} (map-value inc {:a 1 :b 2}))))

(deftest test-day-four-part-one
  (is (= 39422 (day-four-part-one input))))
