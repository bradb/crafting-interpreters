(ns lox.pprint-test
  (:require [lox.pprint :as lp]
            [lox.scanner :as lsc]
            [clojure.test :refer [deftest is]])
  (:import [lox.expr LiteralExpr UnaryExpr GroupingExpr BinaryExpr]))

(deftest literal-expr-test
  (is (= "nil" (lp/pp (LiteralExpr. nil))))
  (is (= "1" (lp/pp (LiteralExpr. 1))))
  (is (= "\"hello, world!\"" (lp/pp (LiteralExpr. "\"hello, world!\"")))))

(deftest grouping-expr-test
  (is (= "(group 12.34)" (lp/pp (GroupingExpr. (LiteralExpr. 12.34))))))

(deftest unary-expr-test
  (is (= "(- 42)", (lp/pp (UnaryExpr. (lsc/token ::lsc/minus "-" nil 1) (LiteralExpr. 42))))))

(deftest binary-expr-test
  (is (= "(+ 1 4)" (lp/pp (BinaryExpr. (lsc/token ::lsc/plus "+" nil 1) (LiteralExpr. 1) (LiteralExpr. 4))))))

(deftest compound-expr-test
  (is (= "(* (- 123) (group 45.67))" (lp/pp (BinaryExpr. (lsc/token ::lsc/star "*" nil 1)
                                                         (UnaryExpr.
                                                          (lsc/token ::lsc/minus "-" nil 1)
                                                          (LiteralExpr. 123))
                                                         (GroupingExpr. (LiteralExpr. 45.67)))))))
