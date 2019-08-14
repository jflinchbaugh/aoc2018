(ns aoc2018.day-five)

(def input (->> "src/aoc2018/day_five.txt" slurp .trim (map str)))

(def reacts? (memoize (fn [[x y]]
                (and
                  (.equalsIgnoreCase x y)
                  (not= x y)))))

(defn collapse [input]
  (if (empty? input)
    input
    (let [c1 (->> input
                  (partition-all 2)
                  (remove reacts?)
                  flatten)
          c2 (->> c1
                  rest
                  (partition-all 2)
                  (remove reacts?)
                  flatten)
          res (doall (concat [(first c1)] c2))]
      (if (= input res)
        res
        (recur res)))))

(count (collapse input))

(def my-chars (map (comp str char) (range (int \a) (inc (int \z)))))

(apply min (pmap (fn [c]
                   (->> input
                        (remove #(.equalsIgnoreCase c %))
                        collapse
                        count)) my-chars))

