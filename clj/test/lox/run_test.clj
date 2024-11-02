(ns lox.run-test
  (:require  [clojure.test :refer [deftest is are]]
             [lox.run :as lr]))

(deftest run-primary-test
  (are [x y] (= x (lr/run (str "print " y ";")))
    "true" "true"
    "45" "45"
    "hello, world" "\"hello, world\""
    "nil" "nil"
    "false" "false"
    "42" "(42)"))

(deftest run-unary-test
  (is (= "-67" (lr/run "print -67;")))
  (is (= "42" (lr/run "print --42;")))
  (is (= "15" (lr/run "print 15;")))
  (is (= "true" (lr/run "print !false;"))))

(deftest run-binary-test
  (are [x y] (= x (lr/run (str "print " y ";")))
    "2" "1 + 1"
    "0" "1 - 1"
    "5" "20 / 4"
    "200" "5 * 40"
    "true" "10 > 9"
    "false" "10 >= 11"
    "true" "1918 < 7171"
    "false" "10 <= 0"
    "false" "\"hello\" != \"hello\""
    "true" "\"hello\" == \"hello\""))

(deftest run-string-concat-test
  (are [x y] (= x (lr/run (str "print " y ";")))
    "foobar" "\"foo\" + \"bar\""
    "hello, world!" "\"hello,\" + \" world!\""))

(deftest complex-exprs-test
  (are [x y] (= x (lr/run (str "print " y ";")))
    "6" "1 + 2 + 3"
    "24" "4 * 3 * (1 + 1)"
    "true" "8 + 12 == 36 - 16"
    "false" "8 + 12 == 36 - 17"
    "342" "7 + (12 * (4 - 1)) + (300 - 1)"
    "false" "(10 > 12) == (88 < 100)"
    "true" "(10 < 12) == (88 < 100)"))

(deftest run-print-statement-test
  (are [x y] (is (= x (with-out-str (lr/run y))))
    "1.0\n" "print 1;"
    "36.0\n" "print 6*6;"
    "hello, world!\n" "print \"hello, world!\";"
    "true\n" "print true;"
    "nil\n" "print nil;"
    "foobar\n" "print \"foo\" + \"bar\";"
    "hi\nthere\n" "print \"hi\"; print \"there\";"))

(deftest run-expressions-statement-test
  (is (= "" (with-out-str (lr/run "1;")))))

(deftest runtime-errors-test
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"must be numbers" (lr/run "print 1 * false;"))))

(deftest run-parser-error-test
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"error parsing input" (lr/run "print (42;"))))

(deftest run-lexer-error-test
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"error parsing input" (lr/run "print \"foo;"))))

(deftest synchronise-after-parse-error-test
  (is false))
