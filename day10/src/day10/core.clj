(ns day10.core)


(defn get-differences [numbers]
  (let [l1 (butlast numbers)
        l2 (rest numbers)]
    (map - l2 l1)))

(defn solve-1 [numbers]
  (let [numbers (vec (sort numbers))
        numbers (into [0] numbers)
        differences (get-differences numbers)
        d1 (count (filter #(= 1 %) differences))
        d3 (count (filter #(= 3 %) differences))]
    (* d1 (inc d3))))

;(defn do-get-paths [start end numbers path]
;  (if (empty? numbers)
;    (when (= (last path) end)
;      [path]
;      )
;    (let [candidate (first numbers)]
;      (when (<= (- candidate start) 3)
;        (let [found
;              (do-get-paths candidate end (rest numbers) (conj path candidate))
;              found-2
;              (do-get-paths start end (rest numbers) path)
;              all (into found found-2)]
;          all))))
;  )
;
;(defn get-paths [start numbers]
;  (do-get-paths start (last numbers) numbers [])
;  )
;
;(defn solve-2 [numbers]
;  (let [numbers (vec (sort numbers))
;        paths (get-paths 0 numbers)]
;    (count paths))
;  )
(declare m-count-paths)

(defn count-paths [tree start end]
  (if (= start end)
    1
    (let [children (get tree start)]
      (reduce +
              (for [child children]
                (m-count-paths tree child end))))))

(def m-count-paths
  (memoize count-paths))

(defn build-tree [numbers]
  (let [end (last numbers)
        numbers (into numbers [(+ end 4) (+ end 8)])
        nodes (for [index (range (- (count numbers) 3))]
                (let [n0 (nth numbers index)
                      n1 (nth numbers (+ 1 index))
                      n2 (nth numbers (+ 2 index))
                      n3 (nth numbers (+ 3 index))
                      n1f (<= (- n1 n0) 3)
                      n2f (<= (- n2 n0) 3)
                      n3f (<= (- n3 n0) 3)
                      node (if n1f {n0 [n1]} {n0 []})
                      node (if n2f (update node n0 conj n2) node)
                      node (if n3f (update node n0 conj n3) node)]
                  node))]
    (apply merge nodes)))

(defn solve-2 [numbers]
  (let [numbers (vec (sort numbers))
        numbers (into [0] numbers)
        end (last numbers)
        tree (build-tree numbers)
        ]
    (count-paths tree 0 end)
    ))