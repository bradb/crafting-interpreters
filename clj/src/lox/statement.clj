(ns lox.statement)

(defrecord PrintStatement [expr])
(defrecord ExpressionStatement [expr])

(defrecord BinaryExpression [oper left right])
(defrecord GroupingExpression [expr])
(defrecord UnaryExpression [oper right])
(defrecord LiteralExpression [val])