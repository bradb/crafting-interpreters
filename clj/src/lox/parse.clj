(ns lox.parse
  "Parser for the Lox programming language."
  (:require [lox.scanner :as s])
  (:import [lox.expr GroupingExpr BinaryExpr UnaryExpr LiteralExpr]))

(def primary? #{::s/number
                ::s/true
                ::s/false
                ::s/nil
                ::s/string})

(defn- primary
  [tokens]
  (when-let [pt (first tokens)]
    (if (primary? (:type pt))
      (let [literal (case (:type pt)
                      ::s/true
                      true

                      ::s/false
                      false

                      (:literal pt))]
        [(LiteralExpr. literal) (rest tokens)])
      [nil tokens])))

(defn parse
  "Map a coll of tokens to an AST.

  The root of the AST is an expr from lox.expr, e.g. GroupingExpr, BinaryExpr, etc."
  [tokens]
  (let [[t rest-tokens] (primary tokens)]
    t))


