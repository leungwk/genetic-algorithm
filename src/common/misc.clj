(ns common.misc
  (:require [clojure.contrib.string :only (split)]))

;; '#' does an autogensym on the symbol it is appended to
;; '~' is unquote
;; '~@' is unquote splice
(defmacro debug
  [expr]
  `(let [result# ~expr]
     (println '~expr "=>" result#)
     (flush)
     result#))

(defmacro unless [condition & body]
  `(when (not ~condition)
     ~@body))

(defn class-exists?
  "Fully qualified names are required"
  [cname]
  (try
   (do (Class/forName cname)
       true)
   (catch ClassNotFoundException e false)))

(defn palindrome? [num]
  (let [s (seq (str num))]
    (= s (reverse s))))

(defn make-palindrome
  "make palindrome by mirroring string starting at last character
ex. (make-palindrome 'string') => 'gnirtsstring'"
  [s]
  (reduce str (concat (reduce str (reverse s)) s)))

(defn take-to-first
  "Returns a lazy sequence of successive items from coll up to
  and including the point at which it (pred item) returns true.
  pred must be free of side-effects.

  src: http://www.mail-archive.com/clojure@googlegroups.com/msg25706.html"
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if-not (pred (first s))
       (cons (first s) (take-to-first pred (rest s)))
       (list (first s))))))

(defn take-including
  "Returns a lazy sequence of successive items from coll up to and
  including the point at which it (pred item) returns false.
  pred must be free of side-effects.

  aka. take-while-inclusive"
  [pred coll]
  (take-to-first #(not (pred %)) coll))

(defn unzip
  "Mimic the behaviour of Python's zip(*coll)

  (unzip '([3 4 5] [3 4 5] [3 4 5]))  =>  ((3 3 3) (4 4 4) (5 5 5))
  (unzip '((\"x\" 1) (\"y\" 2) (\"z\" 3 3.0)))  =>  ((\"x\" \"y\" \"z\") (1 2 3))
  (unzip '((\"x\" 1 1.0) (\"y\" 2) (\"z\" 3 3.0)))  =>  ((\"x\" \"y\" \"z\") (1 2 3))"
  [coll]
  (loop [lst coll, acc ()]
    (if (some empty? lst)
      acc
      (recur (map rest lst)
             (concat acc (list (map first lst)))))))

(defn binary-to-float
  "Convert a binary sequence to float, starting at 2^idx"
  [bits idx]
  (if (empty? bits)
    nil
    (let [nb (count bits)]
      (reduce + (map #(if (= 1 %1) (Math/pow 2 %2) 0)
                     bits
                     (range idx (- idx nb) -1))))))

(defn float-to-binary
  "Convert a float to a binary sequence of nbits with MSB starting at 2^idx. Binary has no sign bit."
  [num idx nbits]
  {:pre [(>= num 0), (>= nbits 0)]}
  (if (= nbits 0)
    ()
    (let [[integral-str frac-str] (clojure.contrib.string/split #"\." (Float/toString num))
          integral (Integer/parseInt integral-str)
          frac (Float/parseFloat (str "0." frac-str))
          nfracbits (- nbits idx 1)
          int-bits (map #(Integer/parseInt %)
                        (drop 1 (clojure.contrib.string/split #"" (Integer/toBinaryString integral))))
          frac-bits (loop [k nfracbits, f frac, acc ()]
                      (if (<= k 0)
                        acc
                        (let [[in-str f-str] (clojure.contrib.string/split #"\." (Float/toString (* f 2)))]
                          (recur (- k 1)
                                 (Float/parseFloat (str "0." f-str))
                                 (concat acc (list (if (= "1" in-str) 1 0)))))))]
      (concat int-bits frac-bits))))