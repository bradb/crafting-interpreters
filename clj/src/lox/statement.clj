(ns lox.statement)

(defrecord Block [declarations])
(defrecord ExpressionStatement [expr])
(defrecord IfStatement [expr then-stmt else-stmt])
(defrecord PrintStatement [expr])
(defrecord VarStatement [identifier expr])
(defrecord WhileStatement [expr stmt])

(defrecord AssignmentExpression [identifier expr])
(defrecord BinaryExpression [oper left right])
(defrecord GroupingExpression [expr])
(defrecord LiteralExpression [val])
(defrecord LogicalExpression [oper left right])
(defrecord UnaryExpression [oper right])
(defrecord VariableExpression [identifier])
