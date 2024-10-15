(ns lox.scanner-test
  (:require  [clojure.test :refer [deftest is]]
             [lox.scanner :as s]))

(deftest single-char-tokens-test
  (is (= [(s/->Token ::s/left-paren "(" nil 1)] (s/scan "(")))
  (is (= [(s/->Token ::s/right-paren ")" nil 1)] (s/scan ")")))
  (is (= [(s/->Token ::s/left-brace "{" nil 1)] (s/scan "{")))
  (is (= [(s/->Token ::s/right-brace "}" nil 1)] (s/scan "}")))
  (is (= [(s/->Token ::s/comma "," nil 1)] (s/scan ",")))
  (is (= [(s/->Token ::s/dot "." nil 1)] (s/scan ".")))
  (is (= [(s/->Token ::s/minus "-" nil 1)] (s/scan "-")))
  (is (= [(s/->Token ::s/plus "+" nil 1)] (s/scan "+")))
  (is (= [(s/->Token ::s/semicolon ";" nil 1)] (s/scan ";")))
  (is (= [(s/->Token ::s/star "*" nil 1)] (s/scan "*"))))

(deftest multiple-single-tokens-test
  (is (= [(s/->Token ::s/left-paren "(" nil 1)
          (s/->Token ::s/right-paren ")" nil 1)]
         (s/scan "()")))
  (is (= [(s/->Token ::s/semicolon ";" nil 1)
          (s/->Token ::s/dot "." nil 1)
          (s/->Token ::s/star "*" nil 1)
          (s/->Token ::s/minus "-" nil 1)]
         (s/scan ";.*-"))))

(deftest one-and-two-character-lexemes-test
  (is (= [(s/->Token ::s/bang "!" nil 1)]
         (s/scan "!")))
  (is (= [(s/->Token ::s/equal "=" nil 1)]
         (s/scan "=")))
  (is (= [(s/->Token ::s/less "<" nil 1)]
         (s/scan "<")))
  (is (= [(s/->Token ::s/greater ">" nil 1)]
         (s/scan ">")))
  (is (= [(s/->Token ::s/bang-equal "!=" nil 1)]
         (s/scan "!=")))
  (is (= [(s/->Token ::s/equal-equal "==" nil 1)]
         (s/scan "==")))
  (is (= [(s/->Token ::s/less-equal "<=" nil 1)]
         (s/scan "<=")))
  (is (= [(s/->Token ::s/greater-equal ">=" nil 1)]
         (s/scan ">="))))

(deftest string-literal-test
  (is false))

(deftest number-literal-test
  (is false))

(deftest identifier-test
  (is false))

(deftest keyword-test
  (is false))

(deftest mix-of-comments-and-ops-test
  ;; example from Crafting Interpreters, pp. 50
  (let [code "//this is a comment
(( )){} // grouping stuff
!*+-/=<> <= == /operators"]
    (is false)))

(deftest errors-test
  (is false))

(deftest ignore-whitespace-test
  (is false))

(deftest ignore-comments-test
  (is false))

(deftest track-token-line-test
  (is false))

(deftest parse-hello-world-test
  (is false))
