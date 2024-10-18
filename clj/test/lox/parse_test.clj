(ns lox.parse-test
  (:require  [clojure.test :refer [deftest is are]]
             [lox.parse :as lp]
             [lox.scanner :as s])
  (:import [lox.expr GroupingExpr BinaryExpr UnaryExpr LiteralExpr]))

(defn- parse
  [s]
  (->> s
       s/scan
       :tokens
       lp/parse))

(deftest parse-primary-test
  (are [x y] (= (parse x) (LiteralExpr. y))
    "1234" 1234.0
    "\"hello, world\"" "hello, world"
    "true" true
    "false" false
    "nil" nil)
  (is false "parens primary expressions not yet tested"))

(deftest parse-unary-test
  (is false))

(deftest parse-factor-test
  (is false))

