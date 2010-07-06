(ns ga.ga
  (:require [clojure.contrib.duck-streams :only (append-spit read-lines)])
  (:require [clojure.contrib.string :only (split)])
  (:use [common.math :only (draw-nr prime? roulette-wheel)])
  (:use [common.misc :only (unzip)])
  (:use [clojure.contrib.seq-utils :only (separate)])
  (:use common.grid)
  (:use incanter.core)
  (:use [incanter.distributions :exclude (roulette-wheel)]))

(def *unimodal-grid*
     (matrix [
 1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
 1  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  1
 1  5 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10  5  1
 1  5 10 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 10  5  1
 1  5 10 15 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 15 10  5  1
 1  5 10 15 20 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 20 15 10  5  1
 1  5 10 15 20 25 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 50 55 55 55 55 55 55 55 55 55 55 55 55 55 55 55 55 55 55 55 50 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 50 55 60 60 60 60 60 60 60 60 60 60 60 60 60 60 60 60 60 55 50 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 50 55 60 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 60 55 50 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 50 55 60 65 70 70 70 70 70 70 70 70 70 70 70 70 70 65 60 55 50 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 75 75 75 75 75 75 75 75 75 75 70 65 60 55 50 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 80 80 80 80 80 80 80 80 75 70 65 60 55 50 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 85 85 85 85 85 85 80 75 70 65 60 55 50 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 90 90 90 90 85 80 75 70 65 60 55 50 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 95 95 90 85 80 75 70 65 60 55 50 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 99 95 90 85 80 75 70 65 60 55 50 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 95 95 90 85 80 75 70 65 60 55 50 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 90 90 90 90 85 80 75 70 65 60 55 50 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 85 85 85 85 85 85 80 75 70 65 60 55 50 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 80 80 80 80 80 80 80 80 75 70 65 60 55 50 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 75 75 75 75 75 75 75 75 75 75 70 65 60 55 50 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 50 55 60 65 70 70 70 70 70 70 70 70 70 70 70 70 70 65 60 55 50 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 50 55 60 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 60 55 50 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 50 55 60 60 60 60 60 60 60 60 60 60 60 60 60 60 60 60 60 55 50 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 50 55 55 55 55 55 55 55 55 55 55 55 55 55 55 55 55 55 55 55 50 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 30 25 20 15 10  5  1
 1  5 10 15 20 25 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 25 20 15 10  5  1
 1  5 10 15 20 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 20 15 10  5  1
 1  5 10 15 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 15 10  5  1
 1  5 10 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 10  5  1
 1  5 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10  5  1
 1  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  1
 1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
] 41))
; (euler-11-func-coord-dir 4 *unimodal-grid*)
; ==> ({:dir se, :prod 8.041275E7, :coord [18 18]} ...
; (conseq-values 4 'se [18 18] *unimodal-grid*)
; ==> (90.0 95.0 99.0 95.0) ; prod = 8.041275E7
; so there is more than one optimal here

(def *euler-11-grid-ga-2*
     (matrix [
  1  1  1  1  2  2  3  3  3  3  2  2  1  1  1  1
  1  1  1  1  2  2  3  3  3  3  2  2  1  1  1  1
  1  1 60 60 60 60 60 60 60 60 60 60 60 60  1  1
  1  1 60  7  7  8  8  9  9  8  8  7  7 60  1  1
  2  2 60  7  7  8  8  9  9  8  8  7  7 60  2  2
  2  2 60  6  6 80 80  0  0 80 80  6  6 60  2  2
  3  3 60  6  6 80  0  0  0  0 80  6  6 60  3  3
  3  3  4  5  5 80  0 50 50  0 80  5  5  4  3  3
  3  3  4  5  5 80  0 50 50  0 80  5  5  4  3  3
  3  3 60  6  6 80  0  0  0  0 80  6  6 60  3  3
  2  2 60  6  6 80 80  0  0 80 80  6  6 60  2  2
  2  2 60  7  7  8  8  9  9  8  8  7  7 60  2  2
  1  1 60  7  7  8  8  9  9  8  8  7  7 60  1  1
  1  1 60 60 60 60 60 60 60 60 60 60 60 60  1  1
  1  1  1  1  2  2  3  3  3  3  2  2  1  1  1  1
  1  1  1  1  2  2  3  3  3  3  2  2  1  1  1  1
] 16))

(defn select-reproducers-3 [nrep pop]
  (let [minfit (reduce min (map #(:fitness %) pop))]
    (cond (> nrep (count pop)) (throw (new Exception "k exceeds coll size"))
          :else (draw-nr nrep pop (map #(+ minfit 1 (:fitness %)) pop)))))

(defn select-reproducers-1 [nrep pop]
  (let [[pop0 popr] (separate #(<= (:fitness %) 0) pop) ; pos
        npop (count pop)
        npopr (count popr)
        npop0 (count pop0)
        poprf (map #(:fitness %) popr)
        diff (- nrep npopr)]
    (cond (> nrep npop) (throw (new Exception "k exceeds coll size"))
          (<= diff 0) (draw-nr nrep popr poprf)
          :else (concat popr (draw-nr diff pop0 (repeat npop0 1))))))

(defn select-reproducers-2 [nrep pop]
  (let [[pop0 popr] (separate #(>= (:fitness %) 0) pop) ; neg
        npop (count pop)
        npopr (count popr)
        npop0 (count pop0)
        poprf (map #(:fitness %) popr)
        diff (- nrep npopr)]
    (cond (> nrep npop) (throw (new Exception "k exceeds coll size"))
          (<= diff 0) (draw-nr nrep popr poprf)
          :else (concat popr (draw-nr diff pop0 (repeat npop0 1))))))

;; (defn mutate-2
;;   [chro]
;;   (reduce merge
;;           (map (fn [k]
;;                  {k ((fn draw-val [d]
;;                        (if (vector? d)
;;                          (map draw-val d)
;;                          (draw d)))
;;                      (k chro))})
;;                (keys chro))))

(defn one-point-crossover [g1 g2] ; assume for now individuals have same structure
  (let [gnf (dissoc (into {} g1) :fitness)
        ks (keys gnf)]
    (map #(reduce merge %)
         (unzip
          (map (fn [k]
                 (let [v1 (k g1)
                       v2 (k g2)]
                   (if-not (vector? v1)
                     (list {k v2} {k v1})
                     (let [cp (rand-int (count v1)) ; todo: generalize (or specify the crossover function in `ga'
                           v1p1 (take cp v1)
                           v1p2 (drop cp v1)
                           v2p1 (take cp v2)
                           v2p2 (drop cp v2)]
                       (list {k (vec (concat v1p1 v2p2))}
                             {k (vec (concat v2p1 v1p2))})))))
               ks)))))

(defn binary-to-decimal [bits idx]
  "Convert a binary sequence to decimal, starting at 2^idx"
  (let [nb (count bits)]
    (reduce + (map #(if (= 1 %1) (Math/pow 2 %2) 0)
                   bits
                   (range idx (- idx nb) -1)))))

(defn mutate-2
  [chro]
  (reduce merge
          (map (fn [k]
                 {k ((fn draw-val [d]
                       (if (vector? d)
                         (map draw-val d)
                         (draw d)))
                     (k chro))})
               (keys chro))))

(defn ga [& options]
  (let [opts (when options (apply assoc {} options))
        numgen (or (:numgen opts) 10000)
        mrate (or (:mrate opts) 0.05)
        nipop (or (:nipop opts) 20)
        nrep (or (:nrep opts) 10)
        nchilds (or (:nchilds opts) 5)
        chro (if (:chro opts) (:chro opts) (throw (new Exception "Unspecified chromosome")))
        ff (if (:ff opts) (:ff opts) (throw (new Exception "Unspecified fitness function")))
        sr (if (:sr opts) (:sr opts) (throw (new Exception "Unspecified select reproducers function")))]
    (let [gen-rand-individual #(let [wc (mutate-2 chro)]
                                 (assoc wc :fitness (ff wc)))
          ipop (repeatedly nipop #(gen-rand-individual))
          timestamp (.format (new java.text.SimpleDateFormat "yyyy-MM-dd'T'hhmmss'.'SSS") (new java.util.Date))
          fpath (str "log/q11_ga_" timestamp ".log")]
      (println "Logging to " fpath)
      (println (str "(plot-ga \"" fpath "\")"))
      (loop [pop ipop, i 0, best {:fitness Double/NEGATIVE_INFINITY}]
        (if (>= i numgen)      
          {:best best
           :curpop (sort-by :fitness > pop)}
          (let [reps (sr nrep pop)
                gen-mutant #(if (< (rand) mrate) (list (gen-rand-individual)) ())
                gen-children #(map (fn [x]
                                     (assoc x :fitness (ff x)))
                                   (let [[p1 p2] (vec (draw-nr 2 reps (repeat nrep 1)))]
                                     (one-point-crossover p1 p2))) ; todo: pass in the chromosome and have it operate at that level
                children (reduce concat
                                 (repeatedly nchilds #(concat (gen-mutant)
                                                              (gen-children))))
                newpop (concat reps children)
                fittest (first (sort-by :fitness > pop))]
            (clojure.contrib.duck-streams/append-spit
             fpath
             (str i "," (incanter.stats/mean (map #(:fitness %) newpop)) "\n"))
            (recur newpop
                   (+ i 1)
                   (if (> (:fitness fittest)
                          (:fitness best))
                     fittest
                     best))))))))

(defn plot-ga
  [fname]
  (let [res (unzip (map #(let [res (clojure.contrib.string/split #"," %)]
                           [(Integer/parseInt (first res))
                            (Double/parseDouble (last res))])
                        (clojure.contrib.duck-streams/read-lines fname)))]
    (incanter.core/view
     (incanter.charts/line-chart
      (first res) (last res)))))

(defstruct chro-grid :dir :orig)
(defn ga-unimodal []
  (let [[nrow ncol] (dim *unimodal-grid*)
        chro (struct-map chro-grid
               :dir (reduce merge (map #(hash-map % 1) *dirs*))
               :orig [(reduce merge (map #(hash-map % 1)
                                         (range nrow)))
                      (reduce merge (map #(hash-map % 1)
                                         (range ncol)))])]
    (ga :numgen 2500
        :mrate 0.05
        :nipop 30
        :nrep 10
        :nchilds 5
        :chro chro
        :ff (fn grid-product-2 [map]
              (reduce * (conseq-values 4 (:dir map) (:orig map) *unimodal-grid*)))
        :sr select-reproducers-1)))

(defstruct chro-rastrigin :bits-x1 :bits-x2)
(defn ga-rastrigin []
  (let [nbits 9
        max (Math/pow 2 nbits)
        chro (struct-map chro-rastrigin
               :bits-x1 (vec (map #(hash-map 0 (- max (bit-shift-left 1 %))
                                             1 (- max (- max (bit-shift-left 1 %)))) (range nbits))) ; {0 max-(2^k), 1 max-(max-2^k)}
               :bits-x2 (vec (map #(hash-map 0 (- max (bit-shift-left 1 %))
                                             1 (- max (- max (bit-shift-left 1 %)))) (range nbits))))]
    (ga :numgen 2500
        :mrate 0.01
        :nipop 10
        :nrep 10
        :nchilds 5
        :chro chro
        :ff (fn neg-rastrigin [{bx1 :bits-x1, bx2 :bits-x2}]
              (let [x1 (binary-to-decimal bx1 2)
                    x2 (binary-to-decimal bx2 2)]   ; todo: get rid of magic number
                (- ; todo: allow choosing of max or min (currently max only)
                 (+ 20 (* x1 x1) (* x2 x2) (* -10 (+ (Math/cos (* 2 Math/PI x1))
                                                     (Math/cos (* 2 Math/PI x2))))))))
        :sr select-reproducers-1)))