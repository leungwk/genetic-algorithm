(ns unittest.ga.ga
  (:use clojure.test ga.ga))

(deftest test-binary-to-decimal
  (is (= 9.40625 (binary-to-decimal '(1 0 0 1 , 0 1 1 0 1) 3))))

(deftest test-rastrigin
  (is (= 0 (rastrigin {:bits-x1 '(0 0 0 0 0 0), :bits-x2 '(0 0 0 0 0 0)}))))