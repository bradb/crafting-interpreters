(ns lox.pprint-test
  (:require [lox.pprint :as lp]
            [lox.scanner :as lsc]
            [clojure.test :refer [deftest is]])
  (:import [lox.statement LiteralExpression UnaryExpression GroupingExpression BinaryExpression]))

(deftest literal-expr-test
  (is (= "nil" (lp/pp (LiteralExpression. nil))))
  (is (= "1" (lp/pp (LiteralExpression. 1))))
  (is (= "\"hello, world!\"" (lp/pp (LiteralExpression. "\"hello, world!\"")))))

(deftest grouping-expr-test
  (is (= "(group 12.34)" (lp/pp (GroupingExpression. (LiteralExpression. 12.34))))))

(deftest unary-expr-test
  (is (= "(- 42)", (lp/pp (UnaryExpression. (lsc/token ::lsc/minus "-" nil 1) (LiteralExpression. 42))))))

(deftest binary-expr-test
  (is (= "(+ 1 4)" (lp/pp (BinaryExpression. (lsc/token ::lsc/plus "+" nil 1) (LiteralExpression. 1) (LiteralExpression. 4))))))

(deftest compound-expr-test
  (is (= "(* (- 123) (group 45.67))" (lp/pp (BinaryExpression. (lsc/token ::lsc/star "*" nil 1)
                                                         (UnaryExpression.
                                                          (lsc/token ::lsc/minus "-" nil 1)
                                                          (LiteralExpression. 123))
                                                         (GroupingExpression. (LiteralExpression. 45.67)))))))
