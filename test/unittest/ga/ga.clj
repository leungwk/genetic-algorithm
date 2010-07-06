(ns unittest.ga.ga
  (:use clojure.test ga.ga))

(deftest test-binary-to-decimal
  (is (= 9.40625 (binary-to-decimal '(1 0 0 1 , 0 1 1 0 1) 3))))

(deftest test-rastrigin
  (is (= 0 (rastrigin {:bits-x1 '(0 0 0 0 0 0), :bits-x2 '(0 0 0 0 0 0)}))))

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