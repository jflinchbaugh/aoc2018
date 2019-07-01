(ns aoc2018.day-five)

(def input (->> "src/aoc2018/day_five.txt" slurp .trim (map str)))

(defn reacts? [[x y]]
  (and
   (.equalsIgnoreCase x y)
   (not= x y)))

(defn collapse [input]
  (let [c1 (->> input
             (partition-all 2)
             (remove reacts?)
             flatten)
        c2 (->> c1
             rest
             (partition-all 2)
             (remove reacts?)
             flatten)
        res (concat [(first c1)] c2)]
    res
    (if (= input res)
      res
      (recur res))))

(count (collapse input))
    
