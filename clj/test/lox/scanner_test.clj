(ns lox.scanner-test
  (:require  [clojure.test :refer [deftest is are]]
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
    (is (= [{:message "Unterminated string: \"foo", :line 1}] errors))))

(deftest number-literal-test
  (let [{:keys [errors tokens]} (s/scan "42")]
    (is (empty? errors))
    (is (= [(t ::s/number "42" (Float/parseFloat "42.0") 1)] tokens)))
  (let [{:keys [errors tokens]} (s/scan "3.14")]
    (is (empty? errors))
    (is (= [(t ::s/number "3.14" (Float/parseFloat "3.14") 1)] tokens))))

(deftest bad-number-leading-dot-test
  (let [{:keys [errors tokens]} (s/scan "42.")]
    (is (empty? errors))
    (is (= [(t ::s/number "42" (Float/parseFloat "42") 1)
            (t ::s/dot "." nil 1)] tokens))))

(deftest bad-number-trailing-dot-test
  (let [{:keys [errors tokens]} (s/scan ".99")]
    (is (empty? errors))
    (is (= [(t ::s/dot "." nil 1)
            (t ::s/number "99" (Float/parseFloat "99") 1)]
           tokens))))

(deftest identifier-test
  (let [ts1 (tokens "foobar")
        ts2 (tokens "_isOpen")
        ts3 (tokens "is_open")
        ts4 (tokens "is_2xx_response")]
    (are [x y] (= x y)
      [(t ::s/identifier "foobar" nil 1)] ts1
      [(t ::s/identifier "_isOpen" nil 1)] ts2
      [(t ::s/identifier "is_open" nil 1)] ts3
      [(t ::s/identifier "is_2xx_response" nil 1)] ts4)))

(deftest reserved-words-test
  (let [ts1 (tokens "else")
        ts2 (tokens "class")
        ts3 (tokens "fun")
        ts4 (tokens "or")
        ts5 (tokens "function")]
    (are [x y] (= x y)
      [(t ::s/else "else" nil 1)] ts1
      [(t ::s/class "class" nil 1)] ts2
      [(t ::s/fun "fun" nil 1)] ts3
      [(t ::s/or "or" nil 1)] ts4
      [(t ::s/identifier "function" nil 1)] ts5)))

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
