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
       lp/parse
       :expr))

(deftest parse-primary-test
  (are [x y] (= (parse x) (LiteralExpr. y))
    "1234" 1234.0
    "\"hello, world\"" "hello, world"
    "true" true
    "false" false
    "nil" nil)
  (is false "parens primary expressions not yet tested"))

(deftest parse-unary-test
  (is (= (UnaryExpr. (s/token ::s/minus "-" nil 1) (LiteralExpr. (Float/parseFloat "1234"))) (parse "-1234")))
  (is (= (UnaryExpr. (s/token ::s/bang "!" nil 1) (LiteralExpr. true)) (parse "!true")))
  (is (= (UnaryExpr. (s/token ::s/bang "!" nil 1)
                     (UnaryExpr. (s/token ::s/bang "!" nil 1)
                                 (LiteralExpr. true))) (parse "!!true"))))

(deftest parse-factor-test
  (is (= (BinaryExpr. (s/token ::s/star "*" nil 1)
                      (LiteralExpr. (Float/parseFloat "2"))
                      (LiteralExpr. (Float/parseFloat "4")))
         (parse "2 * 4")))
  (is (= (BinaryExpr. (s/token ::s/star "*" nil 1)
                      (LiteralExpr. (Float/parseFloat "2"))
                      (BinaryExpr.
                       (s/token ::s/slash "/" nil 1)
                       (LiteralExpr. (Float/parseFloat "4"))
                       (LiteralExpr. (Float/parseFloat "6")) ))
         (parse "2 * 4 / 6"))) )

(deftest parse-term-test
  (is (= (BinaryExpr. (s/token ::s/plus "+" nil 1)
                      (LiteralExpr. (Float/parseFloat "2"))
                      (LiteralExpr. (Float/parseFloat "4")))
         (parse "2 + 4")))
  (is (= (BinaryExpr.
          (s/token ::s/plus "+" nil 1)
          (LiteralExpr. (Float/parseFloat "2"))
          (BinaryExpr. (s/token ::s/slash "/" nil 1)
                       (LiteralExpr. (Float/parseFloat "45"))
                       (LiteralExpr. (Float/parseFloat "622"))))
         (parse "2 + 45 / 622"))))

(deftest parse-comparison-test
  (is (= (BinaryExpr.
          (s/token ::s/greater ">" nil 1)
          (LiteralExpr. (Float/parseFloat "31"))
          (LiteralExpr. (Float/parseFloat "5038")))
         (parse "31 > 5038")))
  (is (= (BinaryExpr.
          (s/token ::s/greater-equal ">=" nil 1)
          (BinaryExpr.
           (s/token ::s/slash "/" nil 1)
           (LiteralExpr. (Float/parseFloat "10"))
           (LiteralExpr. (Float/parseFloat "5")))
          (BinaryExpr.
           (s/token ::s/plus "+" nil 1)
           (LiteralExpr. (Float/parseFloat "3"))
           (LiteralExpr. (Float/parseFloat "2"))))
         (parse "10 / 5 >= 3 + 2"))))

(deftest parse-equality-test
  (is (= (BinaryExpr.
          (s/token ::s/bang-equal "!=" nil 1)
          (LiteralExpr. (Float/parseFloat "5"))
          (LiteralExpr. (Float/parseFloat "3")))
         (parse "5 != 3")))

  (is (=
       (BinaryExpr.
        (s/token ::s/bang-equal "!=" nil 1)
        (BinaryExpr.
         (s/token ::s/plus "+" nil 1)
         (LiteralExpr. (Float/parseFloat "5"))
         (BinaryExpr.
          (s/token ::s/slash "/" nil 1)
          (LiteralExpr. (Float/parseFloat "3"))
          (LiteralExpr. (Float/parseFloat "10"))))
        (BinaryExpr.
         (s/token ::s/minus "-" nil 1)
         (LiteralExpr. (Float/parseFloat "1"))
         (LiteralExpr. (Float/parseFloat "2"))))

       (parse "5 + 3 / 10 != 1 - 2"))))

(deftest syntax-errors-test
  (is false))
