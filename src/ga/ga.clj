(ns ga.ga
  (:require [clojure.contrib.duck-streams :only (append-spit read-lines)])
  (:require [clojure.contrib.string :only (split)])
  (:use [common.math :only (draw-nr prime? average-euclidean-distance)])
  (:use [common.misc :only (unzip)])
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

(defn select-reproducers-4 [nrep pop] ; maximization
  (let [maxfit (reduce max (map #(:fitness %) pop))]
    (cond (> nrep (count pop)) pop
          :else (draw-nr nrep pop (map #(- maxfit -1 (:fitness %)) pop)))))

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

(defn binary-to-float [bits idx]
  "Convert a binary sequence to float, starting at 2^idx"
  (let [nb (count bits)]
    (reduce + (map #(if (= 1 %1) (Math/pow 2 %2) 0)
                   bits
                   (range idx (- idx nb) -1)))))

(defn float-to-binary [num idx nbits]
  "Convert a float to a binary sequence of nbits with MSB starting at 2^idx"
  
)

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
             (let [ss (map #(reduce merge (map (fn [x] {x Float/MIN_VALUE}) %)) ; todo: replace with a minimal, non-zero value based on type
                           (map #(keys %)
                                (ck chro)))]
               {ck (vec (f2 ck pop ss))}))]
    (reduce merge (map f3 (keys chro)))))

(defn ga [& options]                    ; uses sets for everything
  (let [opts (when options (apply assoc {} options))
        numgen (or (:numgen opts) 10000)
        mp (or (:mp opts) 0.05)         ; mutate probability
        cp (or (:cp opts) 0.05)         ; crossover probability
        nipop (or (:nipop opts) 20)
        nbest (or (:nbest opts) 5) ; save nbest individuals from entire search
        nrep (or (:nrep opts) 10)
        nchilds (or (:nchilds opts) 5)
        ichro (if (:ichro opts) (:ichro opts) (throw (new Exception "Unspecified initialization chromosome")))
        ff (if (:ff opts) (:ff opts) (throw (new Exception "Unspecified fitness function")))
        df (if (:df opts) (:df opts) (throw (new Exception "Unspecified diversity function")))
        srf (if (:srf opts) (:srf opts) (throw (new Exception "Unspecified select reproducers function")))
        bsf (if (:bsf opts) (:bsf opts) (throw (new Exception "Unspecified best sorting function")))
        mf (if (:mf opts) (:mf opts) (throw (new Exception "Unspecified mutate function")))
        cf (if (:cf opts) (:cf opts) (throw (new Exception "Unspecified crossover function")))]
                    
    (let [assoc-fitness (fn [x]
                          (assoc x :fitness (ff x)))
          gen-rand-indiv #(assoc-fitness (mf %))
          gen-mutant #(assoc-fitness (mf %))
          gen-init-pop (fn [num] (set (repeatedly num #(gen-rand-indiv ichro))))
          ipop (gen-init-pop nipop)
          popquota (+ nrep (* nchilds 2))
          timestamp (.format (new java.text.SimpleDateFormat "yyyy-MM-dd'T'hhmmss'.'SSS") (new java.util.Date))
          fpath (str "log/q11_ga_" timestamp ".log")]
                                        ;      (println "Logging to " fpath)
      (println (str "(plot-ga \"" fpath "\")"))
      (clojure.contrib.duck-streams/append-spit
       fpath
       (str "gen" ","
            "avgfit" ","
            "diversity"
            "\n"))

      (loop [pop ipop, i 1, chro ichro, best (take nbest (sort-by :fitness bsf ipop))]
        (if (> i numgen)
          {:numgen numgen
           :mp mp
           :cp cp
           :nipop nipop
           :nbest nbest
           :nrep nrep
           :nchilds nchilds
           :ichro chro ; final chro

           :best best
           :curpop (sort-by :fitness bsf pop)}
          (let [reps (srf nrep pop)
                gen-children (fn [re]
                               (map assoc-fitness
                                    (let [nre (count re)
                                          [p1 p2]
                                          (vec (draw-nr 2 re (repeat nre 1)))] ; adjust selection method of parents
                                      (cf p1 p2))))
                mutant-fn #(if (< (rand) mp)
                             #{(gen-mutant chro)}
                             #{})
                children-fn #(if (< (rand) cp)
                               (gen-children reps)
                               #{})
                children (reduce union (repeatedly nchilds #(children-fn)))
                mutant (mutant-fn)
                gen-spawn #(union (mutant-fn) (children-fn)) ; use a different method to select spawn? (ie. something not quite total uniqueness (ie. sets), but also not quite no checking (ie. lists)). In Luke's book (30), this is Join(P,Breed(P))
                tmppop (union reps children mutant)
                newpop (loop [j 0, acc tmppop]
                         (let [diff (- popquota (count acc))]
                                        ;(swank.core/break)
                           (if (or (<= diff 0) (>= i 1000)) ; arbitrary (though could optimize based on mp)
                             acc
                             (recur (+ j 1) (union (gen-spawn) acc)))))

                wchro (redist-chro chro newpop)
                diversity (df wchro newpop)
;                _ (swank.core/break)

]
            (clojure.contrib.duck-streams/append-spit
             fpath
             (str i ","
                  (incanter.stats/mean (map #(:fitness %) newpop)) ","
                  diversity

 "\n"))
                                        ;(println i)
            (recur (if (<= (count newpop) 2) ; restart if newpop too low; select using best solutions so far ; todo: parameterize
                     (union (gen-children (seq best))
                            (gen-init-pop 5)) ; todo: parameterize
                     newpop)
                   (+ i 1)
                   wchro
                   (set (take nbest (sort-by :fitness bsf (union best newpop)))))))))))

(defn plot-ga
  [fname]
  (with-data (read-dataset fname :header true)
    (doto (incanter.charts/line-chart :gen :avgfit :x-label "gen" :y-label "avgfit")
      ;; at least this does not return a
      ;; No method in multimethod 'add-lines*' for dispatch value: class org.jfree.data.category.DefaultCategoryDataset
;      (-> #(incanter.charts/add-lines % :gen :diversity :x-label "gen" :y-label "diversity"))

      view
      incanter.charts/clear-background)
    (doto (incanter.charts/line-chart :gen :diversity :x-label "gen" :y-label "diversity")
      view
      incanter.charts/clear-background)

    ;; an indicator of the complexity of Java's architecture approach
    ;; (let [chart (incanter.charts/line-chart :gen :avgfit :x-label "gen" :y-label "avgfit")
    ;;       plot (-> chart .getPlot)
    ;;       ds (.getDataset plot)
    ;;       rax (.getRangeAxis plot)
    ;;       cir (.getRenderer plot)
    ;;       dax (org.jfree.chart.axis.CategoryAxis.)
    ;;       wplot (org.jfree.chart.plot.CategoryPlot. ds dax rax cir)]
    ;;   ;; returns true ...
    ;;   (.setTickLabelsVisible (.getDomainAxis wplot) false)
    ;;   (.isTickMarksVisible (.getDomainAxis wplot))

    ;;   (doto (org.jfree.chart.JFreeChart. wplot)
    ;;     view))
    ))


(defn blah
  "Count the number of collisions and mean length of collision chains when drawing nrep from dist"
  [nrep dist]
  (loop [i 0, ncacc (), mlaacc (), mlcacc ()] ; num collision, mean chain length (all), mean chain length (collisions)
;    (println i)
    (if (>= i 100)                      ; num of statistical samples
      (println (str "| " nrep " | " (incanter.stats/mean ncacc) " | " (incanter.stats/mean mlaacc) " | " (incanter.stats/mean mlcacc) " |"))
      (let [vs (vals (frequencies (repeatedly nrep #(draw dist))))
            cs (remove #(= 1 %) vs)]
        (recur (+ i 1)
               (cons (reduce + cs) ncacc)
               (cons (incanter.stats/mean vs) mlaacc)
               (cons (incanter.stats/mean cs) mlcacc))))))

(defn blah-2
  "Count the number of collisions, and store the distributions of collision chain lengths when drawing nrep from dist"
  [nrep dist]
  (loop [i 0, ncacc (), mlaacc {}] ; num collision, mean chain length (all), mean chain length (collisions)
                                        ;    (println i)
    (if (>= i 100)                      ; num of statistical samples
      (let [ma (format "%.2f"
                       (float (/ (reduce + (map #(* (key %) (/ (val %) 100)) mlaacc))
                                 (reduce + (map #(key %) mlaacc)))))
            x (dissoc mlaacc 1)
            mc (format "%.2f"
                       (float (/ (reduce + (map #(* (key %) (/ (val %) 100)) x))
                                 (reduce + (map #(key %) x)))))]
        (println
         (str "| " nrep " | " (incanter.stats/mean ncacc) " | "
              ma " | " mc
              (reduce str
                      (map #(str " | "(key %) " | " (format "%.2f" (val %)))
                           (reduce merge
                                   (map #(hash-map (key %)
                                                   (float (/ (val %) 100)))
                                        mlaacc))))
              " |")))
      (let [x (frequencies (repeatedly nrep #(draw dist)))
            vs (frequencies (vals x))
            cs (dissoc vs 1)]
        (recur (+ i 1)
               (cons (reduce + (map #(* (key %) (val %)) cs)) ncacc)
               (merge-with + vs mlaacc))))))

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
        :nchilds 5
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
        mvec (vec (map #(vector [0 1] (/ (bit-shift-left 1 %) ub)) (range nbits))) ; {0 max-(2^k), 1 max-(max-2^k)}
        ;; unlike mchro, assume uniform to simulate not knowing anything
        ichro (struct-map chro-rastrigin
                :bits-x1 ssvec
                :bits-x2 ssvec)]
    (ga :numgen 67
        :mp 0.01
        :cp 1
        :nipop 50
        :nbest 5
        :nrep 25
        :nchilds 5
        :ichro ichro
;        :encf nil 
;        :decf nil ; todo: specalized for this prob
        :ff (fn rastrigin [{bx1 :bits-x1, bx2 :bits-x2}]
              (let [x1 (binary-to-float bx1 2)
                    x2 (binary-to-float bx2 2)] ; todo: get rid of magic number
                (+ 20 (* x1 x1) (* x2 x2) (* -10 (+ (Math/cos (* 2 Math/PI x1))
                                                    (Math/cos (* 2 Math/PI x2)))))))
        :df (fn diversity-euclidean ; range [0,\inf)
              [chro pop]
              (let [lpop (seq pop)
                    vs1 (map #(binary-to-float % 2) (map #(:bits-x1 %) lpop))
                    vs2 (map #(binary-to-float % 2) (map #(:bits-x2 %) lpop))
                    vs (unzip (list vs1 vs2))]
                (average-euclidean-distance vs)))
        :srf select-reproducers-4
        :bsf <
        :mf mutate
        :cf one-point-crossover)))