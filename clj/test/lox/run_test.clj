(ns lox.run-test
  (:require  [clojure.test :refer [deftest is]]
             [lox.run :as lr]))

(deftest run-unary-test
  (is (= "14" (lr/run "81 - 67"))))

(deftest run-syntax-error-test
  (is false))

(deftest run-parse-error-test
  (is false))

(deftest run-binary-test
  (is false))

(deftest run-addition-test
  (is false))

(deftest run-string-concat-test
  (is false))

(deftest run-boolean-test
  (is false))

(deftest runtime-errors-test
  (is false))
