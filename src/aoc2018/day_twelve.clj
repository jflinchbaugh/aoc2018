(ns aoc2018.day-twelve
  (:require [clojure.string :as str])
)

(def initial-state "###......#.#........##.###.####......#..#####.####..#.###..#.###.#..#..#.#..#..#.##...#..##......#.#")

(def transitions
  "
    ..#.# => .
    #..## => #
    ..### => .
    ###.# => .
    #...# => #
    ###.. => #
    .##.# => #
    #..#. => #
    #.##. => #
    ####. => .
    .#.## => #
    ...#. => .
    .#..# => #
    .###. => .
    ##..# => #
    .##.. => #
    .#### => #
    .#.#. => #
    ##### => .
    #.#.# => #
    ...## => #
    ..##. => .
    ....# => .
    ##... => .
    ##.#. => #
    ..#.. => #
    ..... => .
    ##.## => .
    #.### => .
    #.#.. => .
    .#... => #
    #.... => .
  "
)

(defn to-flags
  "map the notation string into flags"
  [s]
  (->>
    s
    (map #(= \# %))
    (into [])
  )
)


(defn pot-sum [gens]
  (let
    [
      mappings (as->
        transitions v
        (str/trim v)
        (str/split v #"\n")
        (map str/trim v)
        (map #(str/split % #" => ") v)
        (map (fn [[i r]] [(to-flags i) (first (to-flags r))]) v)
        (into {} v)
      )
      initial (to-flags initial-state)
      initial-with-room (concat
        (repeat gens false)
        [false false]
        initial
        [false false]
        (repeat gens false)
      )
    ]
    (->>
      (loop
        [
          g gens
          state initial-with-room
        ]
        (if (zero? g)
          (vec state)
          (recur
            (dec g)
            (as->
              state v
              (count v)
              (range v)
              (map inc v)
              (reverse v)
              (map #(take 5 (take-last % state)) v)
              (map #(or (mappings %) false) v)
              (concat [false false] v)
            )
          )
        )
      )
      (map (fn [v f] (if f v 0)) (map #(- % 2 gens) (range)))
      (reduce +)
    )
  )
)

(pot-sum 20)
