(ns common.seq)

(defn ints-seq
  "Return an infinite lazy list of random Bigints of size ndig"
  [ndig]
  {:pre [(>= ndig 1)]}
  (repeatedly
   (fn []
     (bigint
      (apply str (repeatedly ndig
                             (fn [] (rand-int 10))))))))