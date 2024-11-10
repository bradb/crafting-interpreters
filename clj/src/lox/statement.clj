(ns lox.statement)

(defrecord Block [declarations])
(defrecord ExpressionStatement [expr])
(defrecord IfStatement [expr then-stmt else-stmt])
(defrecord PrintStatement [expr])
(defrecord VarStatement [identifier expr])

(defrecord AssignmentExpression [identifier expr])
(defrecord BinaryExpression [oper left right])
(defrecord GroupingExpression [expr])
(defrecord UnaryExpression [oper right])
(defrecord LiteralExpression [val])
(defrecord VariableExpression [identifier])
