(ns util.int)

(defn is-int [string]
  (boolean (re-matches #"-?\d+" string)))
