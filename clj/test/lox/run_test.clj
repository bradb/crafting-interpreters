(ns lox.run-test
  (:require  [clojure.test :refer [deftest is are]]
             [lox.run :as lr]))

(deftest run-primary-test
  (are [x y] (= x (lr/run y))
    "true" "true"
    "45" "45"
    "hello, world" "\"hello, world\""
    "nil" "nil"
    "false" "false"
    "42" "(42)"))

(deftest run-unary-test
  (is (= "-67" (lr/run "-67")))
  (is (= "42" (lr/run "--42")))
  (is (= "15" (lr/run "15")))
  (is (= "true" (lr/run "!false"))))

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

(deftest run-syntax-error-test
  (is false))

(deftest run-parse-error-test
  (is false))
