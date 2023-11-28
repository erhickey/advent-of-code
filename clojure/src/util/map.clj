(ns util.map)

(defn contains-keys [map keys]
  (not (not-every? #(contains? map %) keys)))
