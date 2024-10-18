(ns lox.expr)

(defrecord BinaryExpr [oper left right])
(defrecord GroupingExpr [expr])
(defrecord UnaryExpr [oper right])
(defrecord LiteralExpr [val])
