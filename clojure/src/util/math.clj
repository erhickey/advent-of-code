(ns util.math)

(defn gcd [n k]
  (loop [a n b k]
    (if (zero? b) a (recur b (mod a b)))))

(defn lcm [n k]
  (/ (abs (* n k)) (gcd n k)))
