(ns lox.pprint
  "Pretty-print lox expressions.

  I won't necessarily maintain this for each new expression added, just
  doing the first bit of work on this to get a feel for the data types
  used to represent the AST."
  (:import [lox.expr LiteralExpr GroupingExpr UnaryExpr BinaryExpr]))


(defmulti pp class)

(defmethod pp LiteralExpr
  [literal-expr]
  (let [v (:val literal-expr)]
    (if (nil? v)
      "nil"
      (.toString v))))

(defmethod pp GroupingExpr
  [expr]
  (str "(group " (pp (:expr expr)) ")"))

(defmethod pp UnaryExpr
  [{:keys [oper right] :as _expr}]
  (str "(" (:lexeme oper) " " (pp right) ")"))

(defmethod pp BinaryExpr
  [{:keys [oper left right] :as _expr}]
  (str "(" (:lexeme oper) " " (pp left) " " (pp right) ")"))
