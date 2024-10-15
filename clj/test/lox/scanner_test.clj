(ns lox.scanner-test
  (:require  [clojure.test :refer [deftest is]]
             [lox.scanner :as s]))

(def t s/->Token)

(defn- tokens
  [s]
  (->> s
       s/scan
       :tokens))

(deftest single-char-tokens-test
  (is (= [(t ::s/left-paren "(" nil 1)] (tokens "(")))
  (is (= [(t ::s/right-paren ")" nil 1)] (tokens ")")))
  (is (= [(t ::s/left-brace "{" nil 1)] (tokens "{")))
  (is (= [(t ::s/right-brace "}" nil 1)] (tokens "}")))
  (is (= [(t ::s/comma "," nil 1)] (tokens ",")))
  (is (= [(t ::s/dot "." nil 1)] (tokens ".")))
  (is (= [(t ::s/minus "-" nil 1)] (tokens "-")))
  (is (= [(t ::s/plus "+" nil 1)] (tokens "+")))
  (is (= [(t ::s/semicolon ";" nil 1)] (tokens ";")))
  (is (= [(t ::s/star "*" nil 1)] (tokens "*"))))

(deftest multiple-single-tokens-test
  (is (= [(t ::s/left-paren "(" nil 1)
          (t ::s/right-paren ")" nil 1)]
         (tokens "()")))
  (is (= [(t ::s/semicolon ";" nil 1)
          (t ::s/dot "." nil 1)
          (t ::s/star "*" nil 1)
          (t ::s/minus "-" nil 1)]
         (tokens ";.*-"))))

(deftest one-and-two-character-lexemes-test
  (is (= [(t ::s/bang "!" nil 1)]
         (tokens "!")))
  (is (= [(t ::s/equal "=" nil 1)]
         (tokens "=")))
  (is (= [(t ::s/less "<" nil 1)]
         (tokens "<")))
  (is (= [(t ::s/greater ">" nil 1)]
         (tokens ">")))
  (is (= [(t ::s/bang-equal "!=" nil 1)]
         (tokens "!=")))
  (is (= [(t ::s/equal-equal "==" nil 1)]
         (tokens "==")))
  (is (= [(t ::s/less-equal "<=" nil 1)]
         (tokens "<=")))
  (is (= [(t ::s/greater-equal ">=" nil 1)]
         (tokens ">="))))

(deftest string-literal-test
  (is (= [(t ::s/string "\"foobar\"" "foobar" 1)]
         (tokens "\"foobar\""))))

(deftest string-literal-with-newline-test
  (is (= [(t ::s/string "\"foo\nbar\"" "foo\nbar" 1)]
         (tokens "\"foo\nbar\""))))

(deftest unterminated-string-test
  (let [{:keys [errors tokens]} (s/scan "\"foo")]
    (is (= [] tokens))
    (is (= [{:message "Unrecognised character: \"", :line 1}] errors))))

(deftest number-literal-test
  (is false))

(deftest bad-number-leading-dot-test
  (is false))

(deftest bad-number-trailing-dot-test
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

(deftest track-token-line-multiline-string
  (is false))

(deftest parse-hello-world-test
  (is false))
