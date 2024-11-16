(ns lox.parse-test
  (:require  [clojure.test :refer [deftest is are]]
             [lox.parse :as lp]
             [lox.scanner :as s])
  (:import [lox.statement AssignmentExpression Block PrintStatement
            ExpressionStatement VarStatement GroupingExpression
            BinaryExpression VariableExpression UnaryExpression
            LiteralExpression IfStatement LogicalExpression
            WhileStatement]))

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

(defn ident->token
  ([s]
   (ident->token s 1))
  ([s line-num]
   (s/->Token ::s/identifier s nil line-num)))

(deftest parse-if-statement-test
  (is (= (parse "if (7 == 7) print \"hello\"; else print \"goodbye\";")
         [(IfStatement.
           (BinaryExpression. (s/token ::s/equal-equal "==" nil 1) (LiteralExpression. 7.0) (LiteralExpression. 7.0))
           (PrintStatement. (LiteralExpression. "hello"))
           (PrintStatement. (LiteralExpression. "goodbye")))])))

(deftest parse-if-statement-no-else-test
  (is (= (parse "if (7 == 7) print \"hello\";")
         [(IfStatement.
           (BinaryExpression. (s/token ::s/equal-equal "==" nil 1)
                              (LiteralExpression. 7.0)
                              (LiteralExpression. 7.0))
           (PrintStatement. (LiteralExpression. "hello"))
           nil)])))

(deftest parse-if-missing-then-statement-test
  (is (thrown-with-msg? Exception #"missing expression for statement" (parse "if (7 != 7) ; else print \"goodbye\";"))))


(deftest parse-single-line-while-test
  (is (= (parse "var i = 0;
  while (i <= 10) i = i + 1;
  print i;")
         [(VarStatement. (ident->token "i")
                         (LiteralExpression. 0.0))
          (WhileStatement. (BinaryExpression. (s/token ::s/less-equal "<=" nil 2)
                                              (VariableExpression. (ident->token "i" 2))
                                              (LiteralExpression. 10.0))
                           (ExpressionStatement.
                            (AssignmentExpression. (ident->token "i" 2)
                                                   (BinaryExpression. (s/token ::s/plus "+" nil 2)
                                                                      (VariableExpression. (ident->token "i" 2))
                                                                      (LiteralExpression. 1.0)))))
          (PrintStatement. (VariableExpression. (ident->token "i" 3)))])))

(deftest multi-line-while-test
  (is (= (parse "var i = 1;
while (i < 4) {
print i;
i = i + 1;
}")
         [(VarStatement. (ident->token "i")
                         (LiteralExpression. 1.0))
          (WhileStatement. (BinaryExpression. (s/token ::s/less "<" nil 2)
                                              (VariableExpression. (ident->token "i" 2))
                                              (LiteralExpression. 4.0))
                           (Block.
                            [(PrintStatement. (VariableExpression. (ident->token "i" 3)))
                             (ExpressionStatement.
                              (AssignmentExpression. (ident->token "i" 4)
                                                     (BinaryExpression. (s/token ::s/plus "+" nil 4)
                                                                        (VariableExpression. (ident->token "i" 4))
                                                                        (LiteralExpression. 1.0))))]))])))

(deftest parse-var-decl-statement-test
  (is (= (parse "var x;") [(VarStatement. (ident->token "x")
                                          (LiteralExpression. nil))]))
  (is (= (parse "var x = 1;") [(VarStatement. (ident->token "x")
                                              (LiteralExpression. 1.0))]))
  (is (= (parse "var y = 2; print y;") [(VarStatement. (ident->token "y")
                                                       (LiteralExpression. 2.0))
                                        (PrintStatement. (VariableExpression. (ident->token "y")))]))
  (is (= (parse "var z = 3 * 2;") [(VarStatement. (ident->token "z")
                                                  (BinaryExpression. (s/token ::s/star "*" nil 1)
                                                                     (LiteralExpression. 3.0)
                                                                     (LiteralExpression. 2.0)))])))

(deftest parse-assignment-test
  (is (= (parse "y = 1;") [(ExpressionStatement. (AssignmentExpression. (ident->token "y")
                                                                        (LiteralExpression. 1.0)))]))
  (is (= (parse "x = y = 1;") [(ExpressionStatement. (AssignmentExpression.
                                                      (ident->token "x")
                                                      (AssignmentExpression. (ident->token "y")
                                                                             (LiteralExpression. 1.0))))])))

(deftest parse-and-expr-test
  (is (= (parse "1 or 10;")
         [(ExpressionStatement.
           (LogicalExpression. (s/token ::s/or "or" nil 1)
                               (LiteralExpression. 1.0)
                               (LiteralExpression. 10.0)))])))

(deftest parse-or-expr-test
  (is (= (parse "\"foo\" and \"bar\";")
         [(ExpressionStatement. (LogicalExpression. (s/token ::s/and "and" nil 1)
                                                    (LiteralExpression. "foo")
                                                    (LiteralExpression. "bar")))])))

(deftest parse-and-or-expr-test
  (is (= (parse "1 or \"foo\" and \"bar\";")
         [(ExpressionStatement. (LogicalExpression. (s/token ::s/or "or" nil 1)
                                                    (LiteralExpression. 1.0)
                                                    (LogicalExpression. (s/token ::s/and "and" nil 1)
                                                                        (LiteralExpression. "foo")
                                                                        (LiteralExpression. "bar"))))])))

(deftest parse-block-simple-test
  (is (= (parse "{ print a; }")
         [(Block. [(PrintStatement. (VariableExpression. (ident->token "a")))])])))

(deftest parse-block-missing-closing-brace-test
  (is (thrown-with-msg? Exception #"expected closing brace" (parse "{ print a; "))))

(deftest parse-block-test
  (is (= (parse "var a = 1; { print a; }")
         [(VarStatement. (ident->token "a") (LiteralExpression. 1.0))
          (Block. [(PrintStatement. (VariableExpression. (ident->token "a")))])])))

(deftest parse-nested-blocks-test
  (is (= (parse "var a = 1; var b = 2; { print a; { print b; }}")
         [(VarStatement. (ident->token "a") (LiteralExpression. 1.0))
          (VarStatement. (ident->token "b") (LiteralExpression. 2.0))
          (Block. [(PrintStatement. (VariableExpression. (ident->token "a")))
                   (Block. [(PrintStatement. (VariableExpression. (ident->token "b")))])])])))

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
  (let [{:keys [errors tokens expr]} (-> "var 1;"
                                         s/scan
                                         :tokens
                                         lp/parse)]
    (is false "FIXME"))
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
