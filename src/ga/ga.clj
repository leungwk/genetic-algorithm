(ns ga.ga
  (:require [clojure.contrib.duck-streams :only (append-spit read-lines)])
  (:use [common.math :only (draw-nr prime? average-euclidean-distance)])
  (:use [common.misc :only (unzip binary-to-float float-to-binary)])
  (:use [clojure.contrib.seq-utils :only (separate)])
  (:use common.grid)
  (:use criterium.core) ; for benchmarking
  (:use clojure.set)
  (:use incanter.core)
  (:use [incanter.distributions :exclude (roulette-wheel)])
  (:use [incanter.io :only (read-dataset)]))

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

(defn select-tournament
  "Tournament selection.

pop should be genotype only (no fitness value). ff-encf will convert genotype into phenotype (and handle encoding). bsf indicates if maximizing or minimizing.

src: http://en.wikipedia.org/wiki/Tournament_selection"
  [nrep pop ff-encf bsf]
  {:pre [(>= nrep 1)]}
  (let [t 2
        sf (fn [] (loop [i 2, best (draw pop)]
                    (if (> i t)
                      best
                      (let [cand (draw pop)]
                        (recur (+ i 1) (if (bsf (ff-encf cand)
                                                (ff-encf best))
                                         cand best))))))]
    (repeatedly nrep sf)))

(defn select-reproducers-rank
  "Rank scaling.

  src: http://www.mathworks.se/access/helpdesk/help/toolbox/gads/f6691.html"
  [nrep pop bsf]
  (if (> nrep (count pop))
    pop
    (draw-nr nrep (sort-by :fitness bsf pop) (map #(/ 1 %) (range 1 (+ 1 (count pop)))))))

(defn select-reproducers-4 [nrep pop] ; maximization
  (let [maxfit (reduce max (map #(:fitness %) pop))]
    (cond (> nrep (count pop)) pop
          :else (draw-nr nrep pop (map #(- maxfit (- Float/MIN_VALUE) (:fitness %)) pop)))))

(defn select-reproducers-3 [nrep pop] ; maximization
  (let [minfit (reduce min (map #(:fitness %) pop))]
    (cond (> nrep (count pop)) pop
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

(defn one-point-crossover [parents]
  ;; assume for now individuals have same structure
  {:pre [(>= (count parents) 2)]}
  (let [g1 (first parents)
        g2 (second parents)
        ks (keys g1)]
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

(defn mutate-2
  ([chro]
     (reduce merge
             (map (fn [k]
                    {k ((fn draw-val [d]
                          (if (vector? d)
                            (map draw-val d)
                            (draw d)))
                        (k chro))})
                  (keys chro))))
  ([chro indiv]
     (reduce merge
             (map (fn [k]
                    {k (map (fn [b [ss p]]
                              (if (< (rand) p)
                                (draw (remove #(= b %) ss)) ; draw from sample space without b
                                b))
                            (k indiv) (k chro))})
                  (keys chro)))))

(defn mutate
  [chro]
  (reduce merge
          (map (fn [k]
                 {k ((fn draw-val [d]
                       (if (vector? d)
                         (map draw-val d)
                         (draw d)))
                     (k chro))})
               (keys chro))))

(defn mutate-bits-indiv
  [chro indiv]
  (reduce merge
          (map (fn [k]
                 {k (map (fn [b [ss p]]
                           (if (< (rand) p)
                             (draw (remove #(= b %) ss)) ; draw from sample space without b
                             b))
                         (let [ki (k indiv)]
                           (if (coll? ki)
                             ki
                             (list ki))) (k chro))})
               (keys chro))))

(defn redist-chro
  [chro pop] ; a structmap of a vector of maps
  (let [f1 (fn [ck indiv]
             (map #(hash-map % (:fitness indiv))
                  (ck indiv)))
        f2 (fn [ck pop ss] ; weighted frequency count over the entire population
             (map (fn [lm] (reduce #(merge-with + %1 %2) lm))
                  (unzip (cons ss (map #(f1 ck %) pop)))))
        f3 (fn [ck] ; create new chromosome slot
             (let [ss (map #(reduce merge (map (fn [x] {x 1}) %)) ; todo: replace with a minimal, non-zero value based on type
                           (map #(keys %)
                                (ck chro)))]
               {ck (vec (f2 ck pop ss))}))]
    (reduce merge (map f3 (keys chro)))))

(defn take-first
  "Return the first item in coll where (pred item) is true"
  [pred coll]
  (loop [lst coll]
    (cond (empty? lst) nil
          (pred (first lst)) (first lst)
          :else (recur (rest lst)))))

(defn box-muller
  [mu sdsd]
  (let [[w x y] (take-first (fn [[w x y]] (and (> w 0) (< w 1)))
                            (repeatedly #(let [x (rand)
                                               y (rand)
                                               w (+ (Math/pow x 2)
                                                    (Math/pow y 2))]
                                           [w x y])))
        tmp (Math/sqrt (* -2 (Math/log w) (/ 1 w)))]
    [(+ mu (* sdsd x tmp))
     (+ mu (* sdsd y tmp))]))

(defn ga [& options]
  (let [opts (when options (apply assoc {} options))
        numgen (or (:numgen opts) 10000)
        ;; mr = 1 - cr
;        mr (or (:mr opts) 0.05) ; mutation rate (% of pop to mutate)

        ;; todo: allow for mu-plus-lambda and mu-comma-lambda modes
;        qparent (or (:qparent opts) 0.05)

        pr (or (:pr opts) 0.05) ; parental rate (% of pop to become parents)
        cr (or (:cr opts) 0.05) ; crossover rate (% of pop to breed)
        qpop (or (:qpop opts) 50) ; population quota
        qbest (or (:qbest opts) 5) ; save nbest individuals from entire search
        qelite (or (:qelite opts) 1)

        test-ret (fn [test msg] (if test test (throw (new Exception msg))))
        ichro (test-ret (:ichro opts) "Unspecified initialization chromosome")
        encf (test-ret (:encf opts) "Unspecified encoder function")
        decf (test-ret (:decf opts) "Unspecified decoder function")
        ff (test-ret (:ff opts) "Unspecified fitness function")
        df (test-ret (:df opts) "Unspecified diversity function")
        nf (test-ret (:nf opts) "Unspecified noise function")
        srf (test-ret (:srf opts) "Unspecified select reproducers function")
        bsf (test-ret (:bsf opts) "Unspecified best sorting function")
        mf (test-ret (:mf opts) "Unspecified mutate function")
        cf (test-ret (:cf opts) "Unspecified crossover function")]

    ;; initial error checks
    (test-ret (>= qpop qelite) "Elite quote exceeds population quota")
                    
    (let [ff-encf #(ff (encf %))
          assoc-fitness #(assoc % :fitness (ff-encf %))
          dissoc-fitness #(dissoc % :fitness)
          ipop (repeatedly qpop #(mf ichro)) ; gen-init-pop
          nparents 2

          timestamp (.format (new java.text.SimpleDateFormat "yyyy-MM-dd'T'hhmmss'.'SSS") (new java.util.Date))
          fpath (str "log/q11_ga_" timestamp ".log")]
      (println (str "(plot-ga \"" fpath "\")"))
      (clojure.contrib.duck-streams/append-spit
       fpath
       (str "gen" ","
            "avgfit" ","
            "diversity" ","
            "best"
            "\n"))
      (loop [pop ipop, i 1, chro ichro, best ()]
        (if (> i numgen)
          {:ichro chro ; final chro
           :best best}
          (let [pop-wf (map assoc-fitness pop)
                pop-sorted-wf (sort-by :fitness bsf pop-wf)

                wbest-wf (take qbest
                               (sort-by :fitness bsf
                                        (concat pop-wf
                                                (map assoc-fitness best))))

                ;wbest (map dissoc-fitness wbest-wf); unneeded because assoc drops old value if duplicate key

                parents-wf (srf (Math/round (* pr (count pop)))
                                pop
                                ff-encf
                                bsf)
                parents (map dissoc-fitness parents-wf)
                elites-wf (take qelite pop-sorted-wf)
                nelites (count elites-wf)
                diversity (df chro pop encf)
                ;; after this point, :fitness should not be used at all (todo: try to skip dissoc step)

                nspawn (- qpop nelites)
                spawn (loop [i nspawn, acc ()]
                        (if (<= i 0)
                          acc
                          (if (< (rand) cr)
                            (recur (- i 2)
                                   (concat (cf (repeatedly nparents #(draw parents)))
                                           acc))
                            (recur (- i 1) (cons (mf chro) acc)))))
                newpop (concat spawn elites-wf)]
            (clojure.contrib.duck-streams/append-spit
             fpath
             (str i ","
                  (incanter.stats/mean (map #(:fitness %)
                                            (map assoc-fitness newpop))) ","
                  diversity ","
                  (:fitness (first wbest-wf)) "\n"))
            (println i)

            (recur (if (<= (count newpop) nparents) ; restart if newpop too low
                     (let [ngen (- qpop (count best))]
                       (concat wbest-wf (repeatedly ngen #(mf ichro))))
                     newpop)
                   (+ i 1)
                   chro
                   wbest-wf)))))))

(defn plot-ga
  [fname]
  (with-data (read-dataset fname :header true)
    (doto (incanter.charts/line-chart :gen :avgfit :x-label "gen" :y-label "avgfit")
      view
      incanter.charts/clear-background)
    (doto (incanter.charts/line-chart :gen :diversity :x-label "gen" :y-label "diversity")
      view
      incanter.charts/clear-background)
    (doto (incanter.charts/line-chart :gen :best :x-label "gen" :y-label "best")
      view
      incanter.charts/clear-background)))

(defstruct chro-grid :dir :orig)
(defn ga-unimodal []
  (let [[nrow ncol] (dim *unimodal-grid*)
        ichro (struct-map chro-grid
                :dir [(reduce merge (map #(hash-map % 1) *dirs*))]
                :orig [(reduce merge (map #(hash-map % 1)
                                          (range nrow)))
                       (reduce merge (map #(hash-map % 1)
                                          (range ncol)))])
        mchro (struct-map chro-grid
                :dir [[*dirs* 0.25]] ; use p = 1 for mp to control completely
                :orig [[(range nrow) 0.25]
                       [(range ncol) 0.25]])]
    (ga :numgen 100
        :mp 0.001
        :cp 0.5
        :nipop 30
        :nbest 5
        :nrep 10
;        :nchilds 5
        :ichro ichro
        :mchro mchro
        :ff (fn grid-product-2 [map]
              (reduce * (conseq-values 4
                                       (first (:dir map)) ; since expecting a list
                                       (:orig map) *unimodal-grid*)))
        :df nil
        :srf select-reproducers-3
        :bsf >
        :mf mutate-2
        :cf one-point-crossover)))

(defstruct chro-rastrigin :bits-x1 :bits-x2)
(defn ga-rastrigin []
  (let [nbits 10
        ub (Math/pow 2 nbits)
        ssvec (vec (replicate nbits {0 1, 1 1}))
        ichro (struct-map chro-rastrigin
                :bits-x1 ssvec
                :bits-x2 ssvec)
        magic-idx 2] ; todo: do not use magic
    (ga :numgen 1000
        :pr 0.95
        :cr 0.99
        :qpop 50
        :qbest 5
        :qelite 1
        :ichro ichro
        ;; seems like a waste
        :encf (fn encode [{bx1 :bits-x1, bx2 :bits-x2}]
                (let [x1 (binary-to-float bx1 magic-idx)
                      x2 (binary-to-float bx2 magic-idx)]
                  {:x1 x1,
                   :x2 x2}))
        :decf (fn decode [{x1 :x1, x2 :x2}]
                {:bits-x1 (float-to-binary x1 magic-idx nbits),
                 :bits-x2 (float-to-binary x2 magic-idx nbits)})
        :ff (fn rastrigin [{x1 :x1, x2 :x2}]
              (+ 20 (* x1 x1) (* x2 x2) (* -10 (+ (Math/cos (* 2 Math/PI x1))
                                                  (Math/cos (* 2 Math/PI x2))))))
        :df (fn diversity-euclidean [chro pop encf] ; range [0,\inf)
              (let [vs (map #(let [x (encf %)] ; get phenotype from genotype
                               [(:x1 x) (:x2 x)]) pop)]
                (average-euclidean-distance vs)))
        :nf (fn noise-function [ind div encf]
              (let [{x1 :x1, x2 :x2} (encf ind)
                    [bm1 bm2]
                    (box-muller 0 (/ 1 (max div 0.001)))
                    wx1 (+ bm1 x1)
                    wx2 (+ bm2 x2)]
                {:x1 x1, :x2 x2}))
        :srf select-tournament
        :bsf <
        :mf mutate
        :cf one-point-crossover)))

(comment
(fn rosenbrock [{x1 :x1, x2 :x2}]
              (+ (* (- 1 x1) (- 1 x1))
                 (* 100
                    (- x2 (* x1 x1))
                    (- x2 (* x1 x1)))))
(fn rastrigin [{x1 :x1, x2 :x2}]
              (+ 20 (* x1 x1) (* x2 x2) (* -10 (+ (Math/cos (* 2 Math/PI x1))
                                                  (Math/cos (* 2 Math/PI x2))))))
(fn paraboloid [{x1 :x1, x2 :x2}]
              (+ (* x1 x1)
                 (* x2 x2)))
)
