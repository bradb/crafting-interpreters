(ns lox.scanner-test
  (:require  [clojure.test :refer [deftest is]]
             [lox.scanner :as s]))

(deftest single-char-tokens-test
  (is (= [(s/->Token ::s/left_paren "(" nil 1)] (s/scan "(")))
  (is (= [(s/->Token ::s/right_paren ")" nil 1)] (s/scan ")")))
  (is (= [(s/->Token ::s/left_brace "{" nil 1)] (s/scan "{")))
  (is (= [(s/->Token ::s/right_brace "}" nil 1)] (s/scan "}")))
  (is (= [(s/->Token ::s/comma "," nil 1)] (s/scan ",")))
  (is (= [(s/->Token ::s/dot "." nil 1)] (s/scan ".")))
  (is (= [(s/->Token ::s/minus "-" nil 1)] (s/scan "-")))
  (is (= [(s/->Token ::s/plus "+" nil 1)] (s/scan "+")))
  (is (= [(s/->Token ::s/semicolon ";" nil 1)] (s/scan ";")))
  (is (= [(s/->Token ::s/star "*" nil 1)] (s/scan "*"))))

(deftest multiple-single-tokens-test
  (is (= [(s/->Token ::s/left_paren "(" nil 1)
          (s/->Token ::s/right_paren ")" nil 1)]
         (s/scan "()")))
  (is (= [(s/->Token ::s/semicolon ";" nil 1)
          (s/->Token ::s/dot "." nil 1)
          (s/->Token ::s/star "*" nil 1)
          (s/->Token ::s/minus "-" nil 1)]
         (s/scan ";.*-"))))
