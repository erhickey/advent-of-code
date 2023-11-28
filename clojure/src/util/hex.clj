(ns util.hex)

(defn from-hex [hex]
  (apply str
    (map
      (fn [[x y]] (char (Integer/parseInt (str x y) 16)))
      (partition 2 hex))))

