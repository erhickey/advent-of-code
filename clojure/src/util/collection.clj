(ns util.collection)

(defn in? [coll elem]
  (some #(= elem %) coll))

(defn index-of [item coll]
  (count (take-while (partial not= item) coll)))

(defn occurrences [coll e]
  (count (filter #{e} coll)))
