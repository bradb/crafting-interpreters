(ns lox.pprint
  "Pretty-print lox expressions.

  I won't necessarily maintain this for each new expression added, just
  doing the first bit of work on this to get a feel for the data types
  used to represent the AST."
  (:import [lox.statement LiteralExpression GroupingExpression UnaryExpression BinaryExpression]))


(defmulti pp class)

(defmethod pp LiteralExpression
  [literal-expr]
  (let [v (:val literal-expr)]
    (if (nil? v)
      "nil"
      (.toString v))))

(defmethod pp GroupingExpression
  [expr]
  (str "(group " (pp (:expr expr)) ")"))

(defmethod pp UnaryExpression
  [{:keys [oper right] :as _expr}]
  (str "(" (:lexeme oper) " " (pp right) ")"))

(defmethod pp BinaryExpression
  [{:keys [oper left right] :as _expr}]
  (str "(" (:lexeme oper) " " (pp left) " " (pp right) ")"))
