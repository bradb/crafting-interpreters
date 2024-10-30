(ns lox.parse-test
  (:require  [clojure.test :refer [deftest is are]]
             [lox.parse :as lp]
             [lox.scanner :as s])
  (:import [lox.statement PrintStatement ExpressionStatement
            GroupingExpression BinaryExpression
            UnaryExpression LiteralExpression]))

(defn- parse
  [s]
  (->> s
       s/scan
       :tokens
       lp/parse
       :statements))

(deftest parse-print-statement-test
  (are [x y] (= (parse x) y)
    "print 1;" [(PrintStatement. (LiteralExpression. 1.0))]
    "print \"foo\" + \"bar\";" [(PrintStatement. (BinaryExpression. (s/token ::s/plus "+" nil 1)
                                                                    (LiteralExpression. "foo")
                                                                    (LiteralExpression. "bar")))]))

(deftest parse-expression-statement-test
  (are [x y] (= (parse x) y)
    "1;" [(ExpressionStatement. (LiteralExpression. 1.0))]
    "\"foo\" + \"bar\";" [(ExpressionStatement. (BinaryExpression. (s/token ::s/plus "+" nil 1)
                                                                   (LiteralExpression. "foo")
                                                                   (LiteralExpression. "bar")))]))

(deftest parse-primary-test
  (are [x y] (= (parse x) (LiteralExpression. y))
    "1234" 1234.0
    "\"hello, world\"" "hello, world"
    "true" true
    "false" false
    "nil" nil)
  (is (= (GroupingExpression.
          (LiteralExpression. (Float/parseFloat "42")))
         (parse "(42)"))))

(deftest parse-unary-test
  (is (= (UnaryExpression. (s/token ::s/minus "-" nil 1) (LiteralExpression. (Float/parseFloat "1234"))) (parse "-1234")))
  (is (= (UnaryExpression. (s/token ::s/bang "!" nil 1) (LiteralExpression. true)) (parse "!true")))
  (is (= (UnaryExpression. (s/token ::s/bang "!" nil 1)
                     (UnaryExpression. (s/token ::s/bang "!" nil 1)
                                 (LiteralExpression. true))) (parse "!!true"))))

(deftest parse-factor-test
  (is (= (BinaryExpression. (s/token ::s/star "*" nil 1)
                      (LiteralExpression. (Float/parseFloat "2"))
                      (LiteralExpression. (Float/parseFloat "4")))
         (parse "2 * 4")))
  (is (= (BinaryExpression. (s/token ::s/star "*" nil 1)
                      (LiteralExpression. (Float/parseFloat "2"))
                      (BinaryExpression.
                       (s/token ::s/slash "/" nil 1)
                       (LiteralExpression. (Float/parseFloat "4"))
                       (LiteralExpression. (Float/parseFloat "6"))))
         (parse "2 * 4 / 6"))))

(deftest parse-term-test
  (is (= (BinaryExpression. (s/token ::s/plus "+" nil 1)
                      (LiteralExpression. (Float/parseFloat "2"))
                      (LiteralExpression. (Float/parseFloat "4")))
         (parse "2 + 4")))
  (is (= (BinaryExpression.
          (s/token ::s/plus "+" nil 1)
          (LiteralExpression. (Float/parseFloat "2"))
          (BinaryExpression. (s/token ::s/slash "/" nil 1)
                       (LiteralExpression. (Float/parseFloat "45"))
                       (LiteralExpression. (Float/parseFloat "622"))))
         (parse "2 + 45 / 622"))))

(deftest parse-comparison-test
  (is (= (BinaryExpression.
          (s/token ::s/greater ">" nil 1)
          (LiteralExpression. (Float/parseFloat "31"))
          (LiteralExpression. (Float/parseFloat "5038")))
         (parse "31 > 5038")))
  (is (= (BinaryExpression.
          (s/token ::s/greater-equal ">=" nil 1)
          (BinaryExpression.
           (s/token ::s/slash "/" nil 1)
           (LiteralExpression. (Float/parseFloat "10"))
           (LiteralExpression. (Float/parseFloat "5")))
          (BinaryExpression.
           (s/token ::s/plus "+" nil 1)
           (LiteralExpression. (Float/parseFloat "3"))
           (LiteralExpression. (Float/parseFloat "2"))))
         (parse "10 / 5 >= 3 + 2")))
  (is (= (BinaryExpression.
          (s/token ::s/slash "/" nil 1)
          (GroupingExpression.
           (BinaryExpression.
            (s/token ::s/plus "+" nil 1)
            (LiteralExpression. (Float/parseFloat "2"))
            (LiteralExpression. (Float/parseFloat "45"))))
          (LiteralExpression. (Float/parseFloat "622")))
         (parse "(2 + 45) / 622"))))

(deftest parse-equality-test
  (is (= (BinaryExpression.
          (s/token ::s/bang-equal "!=" nil 1)
          (LiteralExpression. (Float/parseFloat "5"))
          (LiteralExpression. (Float/parseFloat "3")))
         (parse "5 != 3")))

  (is (=
       (BinaryExpression.
        (s/token ::s/bang-equal "!=" nil 1)
        (BinaryExpression.
         (s/token ::s/plus "+" nil 1)
         (LiteralExpression. (Float/parseFloat "5"))
         (BinaryExpression.
          (s/token ::s/slash "/" nil 1)
          (LiteralExpression. (Float/parseFloat "3"))
          (LiteralExpression. (Float/parseFloat "10"))))
        (BinaryExpression.
         (s/token ::s/minus "-" nil 1)
         (LiteralExpression. (Float/parseFloat "1"))
         (LiteralExpression. (Float/parseFloat "2"))))

       (parse "5 + 3 / 10 != 1 - 2"))))

(deftest parsing-errors-test
  (let [{:keys [errors tokens expr]} (-> "(3 + 1"
                                         s/scan
                                         :tokens
                                         lp/parse)]
    (is (not (seq tokens)))
    (is (not (seq expr)))
    (is (= ["missing expected closing ')'"]
           errors)))
  (let [{:keys [errors tokens expr]} (-> "1 / ; 2 + 2"
                                         s/scan
                                         :tokens
                                         lp/parse)]
    (is (=  (:tokens (s/scan " 2 + 2")) tokens))
    (is (not (seq expr)))
    (is (= ["expected expression after '/'"] errors))))
