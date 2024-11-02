(ns lox.statement)

(defrecord PrintStatement [expr])
(defrecord ExpressionStatement [expr])
(defrecord VarStatement [identifier expr])

(defrecord AssignmentExpression [identifier expr])
(defrecord BinaryExpression [oper left right])
(defrecord GroupingExpression [expr])
(defrecord UnaryExpression [oper right])
(defrecord LiteralExpression [val])
(defrecord VariableExpression [identifier])
