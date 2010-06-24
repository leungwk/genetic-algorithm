(ns common.math
  (:use (incanter core stats charts))
  (:require [clojure.contrib.probabilities.finite-distributions :as finite-distributions])
  (:require clojure.contrib.math))

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

(defn roulette-wheel
  "Perform a roulette wheel selection given a list of frequencies"
  [freqs]
  (let [nfreqs (count freqs)
        tot (reduce + freqs)
        dist (map #(double (/ % tot)) freqs)
        rval (double (rand))]
    (loop [acc 0, i 0]
      (let [lb acc, ub (+ acc (nth dist i))]
        (cond (>= (+ i 1) nfreqs) i
              (and (>= rval lb) (< rval ub)) i
              :else (recur ub (+ i 1)))))))

(defn count-words [coll] ;; from http://www.citerus.se/kunskap/pnehm/pnehmartiklar/fromjavatoclojure.5.1fe8f33123572b59ab800023092.html
  (reduce #(merge-with + %1 {%2 1}) {} coll))

(defn select-k-no-replacement-with-probability
  "Select num objects from coll with no replacement each with a given probability (expressed as a frequency count)"
  [num coll freqs]
  (last (nth (iterate (fn [[rcoll rfreqs acc]]
                   (let [widx (roulette-wheel rfreqs)]
                     [(concat (take widx rcoll)  (drop (+ widx 1) rcoll))
                      (concat (take widx rfreqs) (drop (+ widx 1) rfreqs))
                      (cons (nth rcoll widx) acc)]))
                 [coll freqs ()])
            num)))