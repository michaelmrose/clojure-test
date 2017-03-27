(ns clojure-test.core-test
  (:require [clojure.test :refer :all]
            [clojure-test.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(deftest two-in-a-row-expected-result
  (testing "two in a row correctly identified"
    (and
     (is (= false (two-in-a-row [])))
     (is (=  true (two-in-a-row [1 1 2])))
     (is  (=  true (two-in-a-row [1 2 2])))
     (is (=  true (two-in-a-row [0 1 1 2])))
     (is (=  false (two-in-a-row [1 2 3]))))))
