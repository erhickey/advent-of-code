(ns util.vec2d)

(defn grid-create [width height default]
  (vec (repeat width (vec (repeat height default)))))

(defn grid-get [grid x y]
  (get (get grid x) y))

(defn grid-assoc [grid x y value]
  (assoc grid x (assoc (get grid x) y value)))

(defn grid-vals [grid]
  (mapcat identity grid))
