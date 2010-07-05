(ns ga.ga
  (:require [clojure.contrib.duck-streams :only (append-spit read-lines)])
  (:require [clojure.contrib.string :only (split)])
  (:use [common.math :only (draw-nr prime? roulette-wheel)])
  (:use [common.misc :only (unzip)])
  (:use [clojure.contrib.seq-utils :only (separate)])
  (:use common.grid)
  (:use incanter.core)
  (:use [incanter.distributions :exclude (roulette-wheel)]))

(def *euler-11-grid-ga*
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
; (euler-11-func-coord-dir 4 *euler-11-grid-ga*)
; ==> ({:dir se, :prod 8.041275E7, :coord [18 18]} ...
; (conseq-values 4 'se [18 18] *euler-11-grid-ga*)
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

;(defstruct chromosome :fitness :orig :dir)
(defstruct chromosome-2 :fitness :bits)
(defn make-gene [gs opts]
  (let [gsks (keys (dissoc (into {} (struct-map gs)) :fitness))]
    (select-keys opts gsks)))

(defn one-point-crossover
  [g1 g2] ;; assume for now individuals have same structure
  (let [gnf (dissoc (into {} g1) :fitness) ; does not change g1 if :fitness key not present
        gnfks (keys gnf)
        ;; partition
        g1p1 (select-keys g1 (take 1 gnfks))
        g1p2 (select-keys g1 (drop 1 gnfks))
        g2p1 (select-keys g2 (take 1 gnfks))
        g2p2 (select-keys g2 (drop 1 gnfks))
        ;; children
        c1 (merge g1p1 g2p2)
        c2 (merge g2p1 g1p2)]
    (list c1 c2)))

(defn two-point-crossover
  [g1 g2] ;; assume for now individuals have same structure
  (let [gnf (dissoc (into {} g1) :fitness) ; does not change g1 if :fitness key not present
        gnfks (keys gnf)
        ;; partition
        g1p1 (select-keys g1 (take 1 gnfks))
        g1p2 (select-keys g1 (take 1 (drop 1 gnfks)))
        g1p3 (select-keys g1 (drop 2 gnfks))
        g2p1 (select-keys g2 (take 1 gnfks))
        g2p2 (select-keys g2 (take 1 (drop 1 gnfks)))
        g2p3 (select-keys g2 (drop 2 gnfks))
        ;; children
        c1 (merge g1p1 g2p2 g1p3)
        c2 (merge g2p1 g1p2 g2p3)]
    (list c1 c2)))

(defn average-fitness
  "Calculate the average fitness of the population"
  [pop]
  (incanter.stats/mean (map #(:fitness %) pop)))

(defn grid-product [{num :num, dir :dir, orig :orig, grid :grid}] ; ignore any additional entries in input if present
  (reduce * (conseq-values num dir orig grid)))
;; TODO allow both
;; (grid-fitness {:num 4 :dir 'se :orig [18 18] :grid *euler-11-grid-ga*})
;; (grid-fitness :num 4 :dir 'se :orig [18 18] :grid *euler-11-grid-ga*)


(defn grid-sum-threshold [{num :num, dir :dir, thresh :thresh, orig :orig, grid :grid}] ; sum num values from orig in dir on grid if each num <= thresh, otherwise return 0
  (when-let [vals (conseq-values num dir orig grid)]
    (if (every? #(<= % thresh) vals)
      (reduce + vals)
      0)))

(defn prime-bits [{bits :bits}]
  (let [nb (count bits)]
    (reduce + (map (fn [idx]
                     (if (prime? idx)
                       (if (= 1 (nth bits idx)) nb (- nb)) ; big difference if reward > 1?
                       (if (= 0 (nth bits idx)) nb (- nb)))) ; big difference if 1,-1 or 0,-1 (ie. whether we want exact strings, or trying to find primes)
                   (range nb)))))

(defn binary-to-decimal [bits idx]
  "Convert a binary sequence to decimal, starting at 2^idx"
  (let [nb (count bits)]
    (reduce + (map #(if (= 1 %1) (Math/pow 2 %2) 0)
                   bits
                   (range idx (- idx nb) -1)))))

(defstruct chromosome-3 :fitness :bits-x1 :bits-x2)
(defn rastrigin [{bx1 :bits-x1, bx2 :bits-x2}]
  (let [x1 (binary-to-decimal bx1 2)
        x2 (binary-to-decimal bx2 2)]   ; todo: get rid of magic number
    (+ 20 (* x1 x1) (* x2 x2) (* -10 (+ (Math/cos (* 2 Math/PI x1))
                                        (Math/cos (* 2 Math/PI x2)))))))

(defn select-from-sample-spaces ; the `mutate' function
  "map (or structure) of sample spaces to select one value from (assume uniform for now). Preserves the existing mapping structure."
  [sss]
  (reduce merge
          (map (fn [k]
                 (let [ss (k sss)]
                   {k (if (vector? ss) ; TODO deal with nested structures better (OR split up coordinates)
                        (vec (map (fn [el] (draw el)) ss))
                        (draw ss))}))
               (keys sss))))

(defstruct chromosome :ss :dist :vals)
(defn mutate-2
  [chro]
  (let [ss (:ss chro)
        dist (:dist chro)]
    (reduce merge (map ;#(hash-map % (nth (% ss) (draw (% dist)))) ; does not work because (% dist) might be a vector of distributions
                   (fn [k]
                     {k ((fn draw-val [d s]
                           (if (vector? d)
                             (map draw-val d s)
                             (nth s (draw d))))
                         (k dist) (k ss))})
                   (keys ss)))))

(defn ga-2 [& options]
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
      (loop [pop ipop, i 0, best {:fitness Double/NEGATIVE_INFINITY}]
        (if (>= i numgen)      
          [best (sort-by :fitness > pop)]
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
            (clojure.contrib.duck-streams/append-spit fpath (str i "," (average-fitness newpop) "\n"))
            (recur newpop
                   (+ i 1)
                   (if (> (:fitness fittest)
                          (:fitness best))
                     fittest
                     best))))))))

;; pass in a sample space to mutate on
(defn ga [numgen mrate gs sss ff ffops sr] ; use ffops for fitness function
  (let [generate-random-gene #(let [ops (merge (select-from-sample-spaces sss)
                                               ffops)]
                                  (swank.core/break)
                                (assoc (make-gene gs ops)
                                  :fitness (ff ops)))
        ipop (repeatedly 20 #(generate-random-gene))
        timestamp (.format (new java.text.SimpleDateFormat "yyyy-MM-dd'T'hhmmss'.'SSS") (new java.util.Date))
        fpath (str "log/q11_ga_" timestamp ".log")]
    (println "Logging to " fpath)
    (loop [pop ipop, i 0, best {:fitness Double/NEGATIVE_INFINITY}]
      (if (= i numgen)
        [best (sort-by :fitness > pop)]
        (let [nrep 10
              reps (sr nrep pop)
              generate-mutant #(if (< (rand) mrate)
                                 (list (generate-random-gene))
                                 ())
              generate-children #(map (fn [x]
                                        (assoc x :fitness (ff (merge x ffops))))
                                      (let [[p1 p2] (vec (draw-nr 2 reps (repeat nrep 1)))]
                                        (one-point-crossover p1 p2)))
              children (reduce concat
                               (repeatedly 5 #(concat (generate-mutant)
                                                      (generate-children))))
              newpop (concat reps children)
              fittest (first (sort-by :fitness > pop))]
          (clojure.contrib.duck-streams/append-spit fpath (str i "," (average-fitness newpop) "\n"))
          (recur newpop
                 (+ i 1)
                 (if (> (:fitness fittest)
                        (:fitness best))
                   fittest
                   best)))))))

(defn plot-ga
  [fname]
  (let [res (unzip (map #(let [res (clojure.contrib.string/split #"," %)]
                           [(Integer/parseInt (first res))
                            (Double/parseDouble (last res))])
                        (clojure.contrib.duck-streams/read-lines fname)))]
    (incanter.core/view
     (incanter.charts/line-chart
      (first res) (last res)))))

(comment

  (let [g *euler-11-grid-ga*
        [nrow ncol] (dim g)]
    (ga 5000
        0.01
        chromosome        
        {:dir *dirs*,
         :orig [(range nrow) (range ncol)]}
        grid-product
        {:num 4, :grid g}
        select-reproducers-1))

(let [[nrow ncol] (dim *euler-11-grid-ga*)
      chro (struct-map chromosome
             :ss {:dir *dirs*
                  :orig [(range nrow) (range ncol)]} ; now that I think about this, this might be encoded poorly, and bits might be better
             :dist {:dir (reduce merge (map #(hash-map % 1)
                                            (range (count *dirs*)))) ; ie. uniform, and to allow (draw (k (:dist chro))) later
                    :orig [(reduce merge (map #(hash-map % 1)
                                              (range nrow)))
                           (reduce merge (map #(hash-map % 1)
                                              (range ncol)))]}
             :vals {:dir nil, :orig nil})]
  (ga-2 :numgen 2500
        :mrate 0.05
        :nipop 30
        :nrep 10
        :nchilds 5
        :chro chro
        :ff (fn grid-product-2 [map]
              (reduce * (conseq-values 4 (:dir map) (:orig map) *euler-11-grid-ga*)))
        :sr select-reproducers-1))

(let [[nrow ncol] (dim *euler-11-grid-ga-2*)
      chro (struct-map chromosome
             :ss {:dir *dirs*
                  :orig [(range nrow) (range ncol)]} ; now that I think about this, this might be encoded poorly, and bits might be better
             :dist {:dir (reduce merge (map #(hash-map % 1)
                                            (range (count *dirs*)))) ; ie. uniform, and to allow (draw (k (:dist chro))) later
                    :orig [(reduce merge (map #(hash-map % 1)
                                              (range nrow)))
                           (reduce merge (map #(hash-map % 1)
                                              (range ncol)))]}
             :vals {:dir nil, :orig nil})]
  (ga-2 :numgen 2500
        :mrate 0.05
        :nipop 30
        :nrep 10
        :nchilds 5
        :chro chro
        :ff (fn grid-sum-threshold [map]
              (when-let [vals (conseq-values 4 (:dir map) (:orig map) *euler-11-grid-ga-2*)]
                (if (every? #(<= % 50) vals)
                  (reduce + vals)
                  0)))
        :sr select-reproducers-1))

  (let [g *euler-11-grid-ga-2*
        [nrow ncol] (dim g)]
    (ga 10000
        0.05
        chromosome        
        {:dir *dirs*,
         :orig [(range nrow) (range ncol)]}
        grid-sum-threshold
        {:num 4, :grid g, :thresh 50}
        select-reproducers-1))

  (ga 100
      0.50
      chromosome-2
      {:bits (vec (repeat 64 [0 1]))}
      prime-bits
      {}
      select-reproducers-1)

;; (defstruct chromosome :fitness :ss :dist :vals)
;; (struct-map chromosome
;;   :fitness Double/NEGATIVE_INFINITY
;;   :ss (repeat 8 [0 1])
;;   :dist (map #(bit-shift-left 1 %) (range 8))
;;   :vals (repeat 8 0))

  ;; TODO fix
  (ga 1000
      0.05
      chromosome-3
      ;; too many lines, and it still requires an extra call to get the result
      ;; {:bits (nth (iterate (fn [[i m]]
      ;;                         [(inc i) (merge m {i (bit-shift-left 1 i)})])
      ;;                      [0 {0 1}])
      ;;             5)}

      ;; 4 2 1 1/2 1/4 ...
;      {:bits-x1 (reduce merge (map #(hash-map % (bit-shift-left 1 %)) (range 8)))
;       :bits-x2 (reduce merge (map #(hash-map % (bit-shift-left 1 %)) (range 8)))}
      (let [brv [0 1]  ; if not a map, implicitly assume uniform. If a map such as {0 1, 1 1} is provide, draw according to that distribution
            nbits 8]
        {:bits-x1 (map #(vector brv (bit-shift-left 1 %)) (range nbits))})
      #(- (rastrigin %))
      {}
      select-reproducers-2)
  


  )