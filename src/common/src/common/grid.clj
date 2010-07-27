(ns common.grid
  (:use incanter.core))

(def *dirs* '(n ne e se s sw w nw))

(defn on-grid?
  [coord grid]
  (let [[nrow ncol] (dim grid)
        [x y] coord]
    (not (or (< x 0) (>= x nrow)
             (< y 0) (>= y ncol)))))

(defn next-coord
  "Get next coordinate in dir (one of 8 cardinals) starting at orig"
  [dir orig]
  (let [[x y] orig]
    (cond (= dir 'n)  [(- x 1) y]
          (= dir 'ne) [(- x 1) (+ y 1)]
          (= dir 'e)  [x       (+ y 1)]
          (= dir 'se) [(+ x 1) (+ y 1)]
          (= dir 's)  [(+ x 1) y]
          (= dir 'sw) [(+ x 1) (- y 1)]
          (= dir 'w)  [x       (- y 1)]
          (= dir 'nw) [(- x 1) (- y 1)]
          :else (throw (Exception. (str dir " is an invalid direction"))))))

(defn conseq-coords
  "Return num consecutive coordinates in direction dir and starting at orig (inclusive)."
  [num dir orig]
  (take num (iterate #(next-coord dir %) orig)))

(defn conseq-values
  "Return num consecutive elements in direction dir from orig (inclusive) on grid. Return () if not enough elements available, and throw error if coord not on grid."
  [num dir orig grid]
  {:pre [(on-grid? orig grid), (>= num 0)]}
  (let [coords (conseq-coords num dir orig)]
    (if (not-every? #(on-grid? % grid) coords)
      ()
      (map (fn [[x y]] (sel grid x y)) coords))))