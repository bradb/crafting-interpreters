(ns lox.interpret
  (:require [lox.scanner :as s])
  (:import [lox.expr UnaryExpr GroupingExpr BinaryExpr LiteralExpr]))

(defmulti eval-expr class)

(defmethod eval-expr UnaryExpr
  [{:keys [oper right] :as _expr}]
  (if (= ::s/minus (:type oper))
    (- (eval-expr right))
    (eval-expr right)))

(defmethod eval-expr LiteralExpr
  [{:keys [val] :as _expr}]
  val)

(defn interpret
  "Recursively evaluate a Lox abstract syntax tree, `ast`.

  The root of the tree is a lox.expr."
  [ast]
  (eval-expr ast))
