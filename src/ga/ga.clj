(ns ga.ga
  (:require [clojure.contrib.duck-streams :only (append-spit read-lines)])
  (:require [clojure.contrib.string :only (split)])
  (:use [common.math :only (draw-nr prime?)])
  (:use [common.misc :only (unzip)])
  (:use [clojure.contrib.seq-utils :only (separate)])
  (:use common.grid)
  (:use incanter.core)
  (:use criterium.core) ; for benchmarking
  (:use clojure.set)
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

(defn binary-to-decimal [bits idx]
  "Convert a binary sequence to decimal, starting at 2^idx"
  (let [nb (count bits)]
    (reduce + (map #(if (= 1 %1) (Math/pow 2 %2) 0)
                   bits
                   (range idx (- idx nb) -1)))))

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

(defn ga-2 [& options] ; uses sets for everything
  (let [opts (when options (apply assoc {} options))
        numgen (or (:numgen opts) 10000)
        mp (or (:mp opts) 0.05) ; mutate probability
        cp (or (:cp opts) 0.05) ; crossover probability
        nipop (or (:nipop opts) 20)
        nbest (or (:nbest opts) 5) ; save nbest individuals from entire search
        nrep (or (:nrep opts) 10)
        nchilds (or (:nchilds opts) 5)
        ichro (if (:ichro opts) (:ichro opts) (throw (new Exception "Unspecified initialization chromosome")))
        mchro (if (:mchro opts) (:mchro opts) (throw (new Exception "Unspecified mutation chromosome")))
        ff (if (:ff opts) (:ff opts) (throw (new Exception "Unspecified fitness function")))
        srf (if (:srf opts) (:srf opts) (throw (new Exception "Unspecified select reproducers function")))
        bsf (if (:bsf opts) (:bsf opts) (throw (new Exception "Unspecified select best sorting function")))
        mif (if (:mif opts) (:mif opts) (throw (new Exception "Unspecified select initial mutate function")))
        mmf (if (:mmf opts) (:mmf opts) (throw (new Exception "Unspecified select mutant mutate function")))]
                    
    (let [gen-rand-indiv #(let [wc (mif ichro)]
                            (assoc wc :fitness (ff wc)))
          gen-mutant (fn [ind] (let [wc (mmf mchro ind)]
                                 ;(swank.core/break)
                                 (assoc wc :fitness (ff wc))))
          gen-init-pop (fn [num] (set (repeatedly num #(gen-rand-indiv))))
          ipop (gen-init-pop nipop)
          popquota (+ nrep (* nchilds 2))
          timestamp (.format (new java.text.SimpleDateFormat "yyyy-MM-dd'T'hhmmss'.'SSS") (new java.util.Date))
          fpath (str "log/q11_ga_" timestamp ".log")]
;      (println "Logging to " fpath)
      (println (str "(plot-ga \"" fpath "\")"))
      (loop [pop ipop, i 0, best (take nbest (sort-by :fitness bsf ipop))]
        (if (>= i numgen)
          {:numgen numgen
           :mp mp
           :cp cp
           :nipop nipop
           :nbest nbest
           :nrep nrep
           :nchilds nchilds
           :ichro ichro
           :mchro mchro

           :best best
           :curpop (sort-by :fitness bsf pop)}
          (let [reps (srf nrep pop)
                gen-children (fn [re]
                               (map (fn [x]
                                      (assoc x :fitness (ff x)))
                                    (let [nre (count re)
                                          [p1 p2]
                                          (vec (draw-nr 2 re (repeat nre 1)))]
                                      (one-point-crossover p1 p2))))
                mutant-fn #(if (< (rand) mp)
                             #{(gen-mutant (draw reps))}
                             #{})
                children-fn #(if (< (rand) cp)
                               (gen-children reps)
                               #{})
                children (reduce union (repeatedly nchilds #(children-fn)))
                mutant (mutant-fn)
                gen-spawn #(union (mutant-fn) (children-fn)) ; use a different method to select spawn? (ie. something not quite total uniqueness (ie. sets), but also not quite no checking (ie. lists)). In Luke's book (30), this is Join(P,Breed(P))
                tmppop (union reps children mutant)
                newpop (loop [i 0, acc tmppop]
                         (let [diff (- popquota (count acc))]
                           ;(swank.core/break)
                           (if (or (<= diff 0) (>= i 1000)) ; arbitrary (though could optimize based on mp)
                             acc
                             (recur (+ i 1) (union (gen-spawn) acc)))))]
            (clojure.contrib.duck-streams/append-spit
             fpath
             (str i "," (incanter.stats/mean (map #(:fitness %) newpop)) "\n"))
            ;(println i)
            (recur (if (<= (count newpop) 2) ; restart if newpop too low; select using best solutions so far ; todo: parameterize
                     (union (gen-children (seq best))
                            (gen-init-pop 5)) ; todo: parameterize
                     newpop)
                   (+ i 1)
                   (set (take nbest (sort-by :fitness bsf (union best newpop)))))))))))

(defn ga [& options] ; does not use sets for everything
  (let [opts (when options (apply assoc {} options))
        numgen (or (:numgen opts) 10000)
        mp (or (:mp opts) 0.05) ; mutate probability
        cp (or (:cp opts) 0.05) ; crossover probability
        nipop (or (:nipop opts) 20)
        nbest (or (:nbest opts) 5) ; save nbest individuals from entire search
        nrep (or (:nrep opts) 10)
        nchilds (or (:nchilds opts) 5)
        ichro (if (:ichro opts) (:ichro opts) (throw (new Exception "Unspecified initialization chromosome")))
        mchro (if (:mchro opts) (:mchro opts) (throw (new Exception "Unspecified mutation chromosome")))
        ff (if (:ff opts) (:ff opts) (throw (new Exception "Unspecified fitness function")))
        srf (if (:srf opts) (:srf opts) (throw (new Exception "Unspecified select reproducers function")))
        bsf (if (:bsf opts) (:bsf opts) (throw (new Exception "Unspecified select best sorting function")))
        mif (if (:mif opts) (:mif opts) (throw (new Exception "Unspecified select initial mutate function")))
        mmf (if (:mmf opts) (:mmf opts) (throw (new Exception "Unspecified select mutant mutate function")))]
                    
    (let [gen-rand-indiv #(let [wc (mif ichro)]
                            (assoc wc :fitness (ff wc)))
          gen-mutant (fn [ind] (let [wc (mmf mchro ind)]
                                 ;(swank.core/break)
                                 (assoc wc :fitness (ff wc))))
          gen-init-pop (fn [num] (repeatedly num #(gen-rand-indiv)))
          ipop (gen-init-pop nipop)
          popquota (+ nrep (* nchilds 2))
          timestamp (.format (new java.text.SimpleDateFormat "yyyy-MM-dd'T'hhmmss'.'SSS") (new java.util.Date))
          fpath (str "log/q11_ga_" timestamp ".log")]
;      (println "Logging to " fpath)
      (println (str "(plot-ga \"" fpath "\")"))
      (loop [pop ipop, i 0, best (set (take nbest (sort-by :fitness bsf ipop)))]
          
        (if (>= i numgen)
          {:numgen numgen
           :mp mp
           :cp cp
           :nipop nipop
           :nbest nbest
           :nrep nrep
           :nchilds nchilds
           :ichro ichro
           :mchro mchro

           :best best
           :curpop (sort-by :fitness bsf pop)}
          (let [reps (srf nrep pop)
                gen-children (fn [re]
                               (map (fn [x]
                                      (assoc x :fitness (ff x)))
                                    (let [nre (count re)
                                          [p1 p2]
                                          (vec (draw-nr 2 re (repeat nre 1)))]
                                      (one-point-crossover p1 p2))))
                mutant-fn #(if (< (rand) mp)
                             (list (gen-mutant (draw reps)))
                             ())
                children-fn #(if (< (rand) cp)
                               (gen-children reps)
                               ())
                children (reduce concat (repeatedly nchilds #(children-fn)))
                mutant (mutant-fn)
                gen-spawn #(concat (mutant-fn) (children-fn))
                tmppop (set (concat reps children mutant)) ; enforces diversity
                newpop (loop [i 0, acc tmppop]
                         (let [diff (- popquota (count acc))]
                           ;(swank.core/break)
                           (if (or (<= diff 0) (>= i 1000)) ; arbitrary (though could optimize based on mp)
                             acc
                             (recur (+ i 1) (union (set (gen-spawn)) acc)))))]
            (clojure.contrib.duck-streams/append-spit
             fpath
             (str i "," (incanter.stats/mean (map #(:fitness %) newpop)) "\n"))
            ;(println i)
            (recur (if (<= (count newpop) 2) ; restart if newpop too low; select using best solutions so far ; todo: parameterize
                     (set (concat (gen-children (seq best))
                                  (gen-init-pop 5))) ; todo: parameterize
                     newpop)
                   (+ i 1)
                   (set (take nbest (sort-by :fitness bsf (union best newpop)))))))))))

(comment
"
  (report-result (quick-bench (ga-unimodal))) ; no sets
  Evaluation count             : 6
  Execution time mean          : 25.454667 sec  95.0% CI: (25.452000 sec, 25.463333 sec)
  Execution time std-deviation : 581.296484 ms  95.0% CI: (579.207735 ms, 586.476938 ms)

  (report-result (quick-bench (ga-unimodal))) ; sets for everything
  WARNING: Final GC required 2.708388131525833 % of runtime
  Evaluation count             : 6
  Execution time mean          : 10.342667 sec  95.0% CI: (10.141333 sec, 10.477333 sec)
  Execution time std-deviation : 8.207178 sec  95.0% CI: (8.164337 sec, 8.216695 sec)
"
)

(defn plot-ga
  [fname]
  (let [res (unzip (map #(let [res (clojure.contrib.string/split #"," %)]
                           [(Integer/parseInt (first res))
                            (Double/parseDouble (last res))])
                        (clojure.contrib.duck-streams/read-lines fname)))]
    (incanter.core/view
     (incanter.charts/line-chart
      (first res) (last res)))))

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
    (ga-2 :numgen 100
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
          :srf select-reproducers-3
          :bsf >
          :mif mutate
          :mmf mutate-bits-indiv)))

(defstruct chro-rastrigin :bits-x1 :bits-x2)
(defn ga-rastrigin []
  (let [nbits 10
        ub (Math/pow 2 nbits)
        ssvec (vec (replicate nbits {0 1, 1 1}))
        mvec (vec (map #(vector [0 1] (/ (bit-shift-left 1 %) ub)) (range nbits))) ; {0 max-(2^k), 1 max-(max-2^k)}
        ;; unlike mchro, assume uniform to simulate not knowing anything
        ichro (struct-map chro-rastrigin
                :bits-x1 ssvec
                :bits-x2 ssvec)
        mchro (struct-map chro-rastrigin
                :bits-x1 mvec
                :bits-x2 mvec)]
    (ga-2 :numgen 100
        :mp 0.01
        :cp 1
        :nipop 50
        :nbest 5
        :nrep 25
        :nchilds 5
        :ichro ichro
        :mchro mchro
        :ff (fn rastrigin [{bx1 :bits-x1, bx2 :bits-x2}]
              (let [x1 (binary-to-decimal bx1 2)
                    x2 (binary-to-decimal bx2 2)] ; todo: get rid of magic number
                (+ 20 (* x1 x1) (* x2 x2) (* -10 (+ (Math/cos (* 2 Math/PI x1))
                                                    (Math/cos (* 2 Math/PI x2)))))))
        :srf select-reproducers-4
        :bsf <
        :mif mutate
        :mmf mutate-bits-indiv)))