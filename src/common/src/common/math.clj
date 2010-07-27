(ns common.math
  (:use [incanter.core :exclude (euclidean-distance)])
  (:use (incanter core stats charts))
  (:require [clojure.contrib.probabilities.finite-distributions :as finite-distributions])
  (:require clojure.contrib.math)
  (:use [clojure.contrib.seq-utils :only (positions)])
  (:use [common.misc :only (unzip)]))

;; from http://clojure-euler.wikispaces.com/The+Optimal+Toolkit
(defn divisible?
  [n d]
  (zero? (rem n d)))

(defn prime?
  "Tests whether a given number is prime."
  [n]
  (cond
    (or (= n 2) (= n 3))          true
    (or (divisible? n 2) (< n 2)) false
    :else                         (let [sqrt-n (Math/sqrt n)]
                                    (loop [i 3]
                                      (cond
                                        (divisible? n i) false
                                        (< sqrt-n i)     true
                                        :else            (recur (+ i 2)))))))

;;;;

(defn prime-factors
  "Factor interger inval into prime factors. Returns a list in descending order."
  ([inval]
     (loop [num inval, cur 2, facts ()]
       (cond (< (Math/sqrt num) cur) (cons num facts) ; num since we are looking for largest
             (divisible? num cur) (recur (/ num cur) cur (cons cur facts))
             :else (recur num (+ cur 1) facts)))))

;; taken from arbscht at http://clojure-euler.wikispaces.com/Problem+005
;(defn abs [x]
;  (if (neg? x) (- x) x))

(defn gcd
  ([] 0)
  ([x] (abs x))
  ([x y] (if (zero? y) (abs x) (recur y (rem x y))))
  ([x y & more] (reduce gcd (gcd x y) more)))

(defn lcm
  ([] 1)
  ([x] (abs x))
;  ([x y] (/ (abs (* x y)) (gcd x y)))
  ([x y] (* (/ (abs x) (gcd x y)) y)) ; since (gcd x y) is a divisor of (abs x)
  ([x y & more] (reduce lcm (lcm x y) more)))

(def PHI 1.61803398874989484820)

(defn fib-n
  "Warning: lost precision imminent"
  [num]
  (int (Math/floor (+ (/ (Math/pow PHI num) (Math/sqrt 5)) 0.5))))

(defn perfect-square?
  "A number that is the square of an integer. That is, its square root does not have any decimal places"
  [num]
  (= (Math/pow (Math/round (Math/sqrt num)) 2) num))

(defn fib?
  "N is a fib number iff (5N^2 +4) or (5N^2 -4) is a perfect square"
  [num]
  (or (perfect-square? (- (* 5 num num) 4))
      (perfect-square? (+ (* 5 num num) 4))))

(defn not-rand-bigint-seq
  "A sequence of not-pseudo-random bigints having nbits bits"
  [nbits]
  (map #(BigInteger. nbits %)
       (repeatedly #(new java.util.Random))))

; (incanter.core/view (incanter.charts/histogram (take 1000 (common.math/rand-bigint-seq 8))))

;; [[http://groups.google.com/group/clojure/browse_thread/thread/9976e6fb06238daa][src]]
;; (defn rand-bigint [#^BigInteger bign, #^java.util.Random rnd]
;;   (let [bits (inc (.bitLength bign))
;;         bigr (BigInteger. bits rnd)]
;;     (-> bign (.multiply bigr) (.shiftRight bits))))

(defn pythagorean-triplet?
  [a b c]
  {:pre [(pos? a), (pos? b), (pos? c)]}
  (= (+ (* a a) (* b b))
     (* c c)))

(defn roulette-wheel-idx
  "Proportionally select from freqs. Returns an index."
  [freqs]
  (let [nfreqs (count freqs)
        tot (reduce + freqs)]
    (if (= tot 0)
      nil
      (let [dist (map #(/ % tot) freqs)
            rval (double (rand))]
        (loop [acc 0, i 0]
          (let [lb acc, ub (+ acc (nth dist i))]
            (cond (>= (+ i 1) nfreqs) i
                  (and (>= rval lb) (< rval ub)) i
                  :else (recur ub (+ i 1)))))))))

(defn count-words [coll] ;; from http://www.citerus.se/kunskap/pnehm/pnehmartiklar/fromjavatoclojure.5.1fe8f33123572b59ab800023092.html
  (reduce #(merge-with + %1 {%2 1}) {} coll))

(defn nths
  "Returns a collection of values for each idx in idxs. Throws an error if any one idx is out of bounds."
  [coll idxs]
  (map #(nth coll %) idxs))

(defn filter-nths
  "Returns a collection of positional values for each element el in coll where (pred el) is true." ; TODO rewrite
  [pred coll]
  (loop [rem coll, acc (), i 0]
    (if (empty? rem)
      acc
      (let [el (first rem)]
        (recur (rest rem)
               (if (pred el) (concat acc (list i)) acc)
               (+ i 1))))))

(defmulti roulette-wheel (fn
                             ([x] [(class x)])
                             ([x y] [(class x) (class y)])))
(defmethod roulette-wheel [java.util.Collection] [coll] (roulette-wheel coll #(identity %)))
(defmethod roulette-wheel [java.util.Collection clojure.lang.Sequential] [coll freqs]
           (if (not (= (count coll) (count freqs)))
             (throw (new Exception "coll and freqs sizes not equal"))
             (let [tot (reduce + freqs)]
               (if (or (= tot 0)
                       (empty? coll))
                 nil
                 (let [small (reduce min freqs)
                       rval (double (rand (+ (min 0 small) tot))) ; `(min 0 small)' otherwise the dist shifts disproportionally r = \frac{x}{\sum x} --> \frac{x+d}{\sum (x+d)} = \frac{x+d}{nd +\sum x}
                       seqcoll (seq coll)]
                   (loop [acc 0, lst seqcoll, lstfreq freqs]
                     (if (empty? lst)
                       (last seqcoll) ; in case lost precision does something weird
                       (let [el (first lst), lb acc, ub (+ acc (first lstfreq))]
                         (if (and (>= rval lb) (< rval ub))
                           el
                           (recur ub (rest lst) (rest lstfreq)))))))))))
(defmethod roulette-wheel [java.util.Collection java.lang.Number] [coll key] (roulette-wheel coll #(get % key)))
(defmethod roulette-wheel [java.util.Collection clojure.lang.AFn] [coll sf]
           (let [tot (reduce + (map sf coll))]
             (if (or (= tot 0)
                     (empty? coll))
               nil
               (let [small (reduce min (map sf coll))
                     rval (double (rand (+ (min 0 small) tot)))]
                 (loop [acc 0, lst coll]
                   (if (empty? lst)
                     (last coll) ; in case lost precision does something weird
                     (let [el (first lst), lb acc, ub (+ acc (sf el))]
                       (if (and (>= rval lb) (< rval ub))
                         el
                         (recur ub (rest lst))))))))))

(defmulti draw-nr (fn
                      ([n x] [(class n) (class x)])
                      ([n x y] [(class n) (class x) (class y)])))
; so Integer doesn't inheret from Long? ...
(defmethod draw-nr [java.lang.Integer clojure.lang.Sequential] [num coll] (draw-nr (bigint num) coll (fn [x] 1)))
(defmethod draw-nr [java.lang.Integer clojure.lang.Sequential clojure.lang.Sequential] [num coll freqs] (draw-nr (bigint num) coll freqs))
(defmethod draw-nr [java.lang.Long clojure.lang.Sequential clojure.lang.Sequential] [num coll freqs] (draw-nr (bigint num) coll freqs))
(defmethod draw-nr [java.math.BigInteger clojure.lang.Sequential clojure.lang.Sequential] [num coll freqs]
           (cond (not (= (count coll) (count freqs))) (throw (new Exception "coll and freqs sizes not equal"))
                 (> num (count coll)) (throw (new Exception "k exceeds coll size"))
                 :else
                 (loop [rlst (seq coll), acc (), lstfreqs freqs]
                   (if (>= (count acc) num)
                     (if (isa? (class coll) clojure.lang.PersistentHashSet)
                       (set acc)
                       acc)
                     (let [widx (roulette-wheel-idx lstfreqs)]
                       (recur (concat (take widx rlst) (drop (+ widx 1) rlst))
                              (cons (nth rlst widx) acc)
                              (concat (take widx lstfreqs) (drop (+ widx 1) lstfreqs))))))))
(defmethod draw-nr [java.lang.Integer java.util.Collection clojure.lang.AFn] [num coll sf]
           (cond (> num (count coll)) (throw (new Exception "k exceeds coll size"))
                 :else
                 (loop [rlst (seq coll), acc ()]
                   (if (>= (count acc) num)
                     (if (isa? (class coll) clojure.lang.PersistentHashSet)
                       (set acc)
                       acc)
                     (let [freqs (map sf rlst)
                           widx (roulette-wheel-idx freqs)]
                       (recur (concat (take widx rlst) (drop (+ widx 1) rlst))
                              (cons (nth rlst widx) acc)))))))

(defn average-euclidean-distance
  "Compute the pairwise distance between vectors vi and vj in coll"
  [coll]
  (let [ef (fn [vi vj]
             (Math/sqrt (reduce + (map #(Math/pow (- %1 %2) 2) vi vj))))]
    (incanter.stats/mean (map (fn [vi]
                                (reduce + (map (fn [vj]
                                                 (ef vi vj)) coll)))
                              coll))))