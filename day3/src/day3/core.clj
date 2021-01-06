(ns day3.core)

(defn translate-line [line]
  (map #(= \# %)
       (filter #(or (= \. %) (= \# %)) line)))

(defn translate-lines [lines]
  (map translate-line (clojure.string/split-lines lines)))

(defn get-point [tree-map [x y]]
  (let [row (nth tree-map y)]
    (nth row x))
  )

(defn count-trees [tree-map [dx dy]]
  (let [depth (count tree-map)
        width (count (first tree-map))
        rows (range 0 depth dy)
        coords (map-indexed #(vector (mod (* %1 dx) width) %2) rows)
        _ (println rows coords)
        points (map #(get-point tree-map %) coords)
        ]
    (count (filter identity points))
    )
  )

(defn solve1 []
  (let [input (slurp "input")
        tree-map (translate-lines input)]
    (count-trees tree-map [3 1]))
  )

(defn solve2 []
  (let [input (slurp "input")
        tree-map (translate-lines input)
        trees (map #(count-trees tree-map %)
                   [[1 1][3 1][5 1][7 1][1 2]])]
    (reduce * trees)
    )
  )
