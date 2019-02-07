(ns aoc2018.day-twelve
  (:require
    [clojure.string :as str]
    [clojure.test :as t]
  )
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

(defn string->flags
  "map the notation string into flags"
  [s]
  (->>
    s
    (map #(= \# %))
    (into [])
  )
)

(defn flags->string [f]
  (->>
    f
    (map #(if % \# \.))
    (str/join)
  )
)

(flags->string [true true false])

(defn flags->indices [start flags]
  ; (map (fn [v f] (if f v 0)) (map #(- % 2 gens) (range)))
  ;(map (fn [v f] (if f v 0)) (map #(- % 2)
  (map #(+ %  start) (range 0 (count flags)))
  ; ))
)

(flags->indices -2 [true false true false true false])

(def mappings
  (as->
    transitions v
    (str/trim v)
    (str/split v #"\n")
    (map str/trim v)
    (map #(str/split % #" => ") v)
    (map (fn [[i r]] [(string->flags i) (first (string->flags r))]) v)
    (into {} v)
  )
)

(if false \# \.)

(defn next-state [state]
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

(def stream (iterate next-state (string->flags initial-state)))

(doseq
  [x  (map flags->string (take 200 stream))]
  (prn x)
)

(defn pot-sum [gens]
  (let
    [
      initial (string->flags initial-state)
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

(string->flags initial-state)

(t/deftest my-test
  (t/is
    (=
      initial-state
      (->>
        initial-state
        string->flags
        flags->string
      )
    )
  )
)

(t/run-all-tests)

(prn mappings)
