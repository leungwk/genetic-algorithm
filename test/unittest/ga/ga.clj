(ns unittest.ga.ga
  (:use clojure.test ga.ga))

(deftest test-binary-to-decimal
  (is (= 9.40625 (binary-to-decimal '(1 0 0 1 , 0 1 1 0 1) 3))))

(deftest test-select-reproducers-1
  (let [pop '({:fitness 0, :dir 's, :orig [40 0]}
              {:fitness 0, :dir 's, :orig [40 0]}
              {:fitness 0, :dir 's, :orig [40 0]}
              {:fitness 0, :dir 's, :orig [40 0]}
              {:fitness 0, :dir 's, :orig [40 0]}
              {:fitness 1, :dir 's, :orig [40 0]}
              {:fitness 2, :dir 's, :orig [40 0]}
              {:fitness 3, :dir 's, :orig [40 0]}
              {:fitness 4, :dir 's, :orig [40 0]}
              {:fitness 5, :dir 's, :orig [40 0]}
              {:fitness 6, :dir 's, :orig [40 0]}
              {:fitness 7, :dir 's, :orig [40 0]}
              {:fitness 8, :dir 's, :orig [40 0]}
              {:fitness 9, :dir 's, :orig [40 0]})]
    (is (every? true? (map #(not (= 0 (:fitness %)))
                          (select-reproducers-1 5 pop))))
    (is (= nil (get (frequencies (map #(:fitness %) (select-reproducers-1 5 pop))) 0))) ; ie. key not found
    (is (= 1 (get (frequencies (map #(:fitness %) (select-reproducers-1 10 pop))) 0)))))

(deftest test-redist-chro
  (let [chro (struct-map chro-rastrigin
               :bits-x1 (vec (replicate 10 {0 1, 1 1, 2 1}))
               :bits-x2 (vec (replicate 10 {0 1, 1 1})))
        pop '({:fitness 1, :bits-x2 (1 0 0 0 0 0 1 1 0 0), :bits-x1 (0 0 0 0 0 0 0 0 0 0)}
              {:fitness 2, :bits-x2 (0 1 0 0 0 1 1 0 1 1), :bits-x1 (0 0 0 0 0 0 0 0 0 0)}
              {:fitness 3, :bits-x2 (0 0 1 0 1 1 1 0 1 0), :bits-x1 (0 0 0 0 0 0 0 0 0 0)}
              {:fitness 4, :bits-x2 (0 0 0 1 1 1 1 1 0 0), :bits-x1 (0 0 0 0 0 0 0 0 0 1)})] ; use integers to avoid floating point addition inaccuracies
    (is (= {:bits-x1 (concat (replicate 9 {0 10, 1 0}) (list {0 6, 1 4}))
            :bits-x2 '({0 9, 1 1}
                       {0 8, 1 2}
                       {0 7, 1 3}
                       {0 6, 1 4}
                       {0 3, 1 7}
                       {0 1, 1 9}
                       {0 0, 1 10}
                       {0 5, 1 5}
                       {0 5, 1 5}
                       {0 8, 1 2})}
           (redist-chro chro pop)))))