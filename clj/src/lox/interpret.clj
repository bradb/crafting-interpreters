(ns lox.interpret
  (:require [lox.scanner :as s])
  (:import [lox.statement AssignmentExpression PrintStatement ExpressionStatement
            VarStatement UnaryExpression GroupingExpression BinaryExpression
            Block VariableExpression LiteralExpression IfStatement
            LogicalExpression WhileStatement]))

(def ^:private ^:dynamic *state* nil)

(declare eval-expr)

(defn- truthy?
  [v]
  (not (or (nil? v) (false? v))))

(defn- declare-variable!
  [k expr]
  (if-let [inner-scope (first *state*)]
    (swap! inner-scope assoc k (eval-expr expr))
    (throw (ex-info "failed to find inner scope for variable declaration"
                    {:runtime-error true}))))

(defn- assign-variable!
  [k expr]
  (loop [inner (first *state*)
         outers (rest *state*)]
    (if inner
      (let [scope @inner]
        (if (contains? scope k)
          (swap! inner assoc k (eval-expr expr))
          (recur (first outers) (rest outers))))
      (throw (ex-info (str "attempt to assign to undeclared variable '" k "'")
                      {:runtime-error true})))))

(defn- variable->value
  [k]
  (loop [inner (first *state*)
         outers (rest *state*)]
    (if inner
      (let [scope @inner]
        (if (contains? scope k)
          (get scope k)
          (recur (first outers) (rest outers))))
      (throw (ex-info (str "reference to undeclared variable '" k "'")
                      {:runtime-error true})))))

(defmulti eval-expr class)

(defmethod eval-expr nil
  [& _]
  nil)

(defmethod eval-expr GroupingExpression
  [{:keys [expr]}]
  (eval-expr expr))

(defmethod eval-expr BinaryExpression
  [{:keys [oper left right] :as expr}]
  (let [le (eval-expr left)
        re (eval-expr right)]
    (case (:type oper)
      ::s/plus
      (cond
        (every? number? [le re])
        (+ le re)

        (every? string? [le re])
        (str le re)

        :else
        (throw
         (ex-info "operands for '+' must both be strings or both be numbers, got "
                  {:runtime-error true, :expr expr})))

      ::s/minus
      (if (every? number? [le re])
        (- le re)
        (throw (ex-info "operands for '-' must be numbers"
                        {:runtime-error true, :expr expr})))

      ::s/star
      (if (every? number? [le re])
        (* le re)
        (throw (ex-info "operands for '*' must be numbers"
                        {:runtime-error true, :expr expr})))

      ::s/slash
      (if (every? number? [le re])
        (/ le re)
        (throw (ex-info "operands for '/' must be numbers"
                        {:runtime-error true, :expr expr})))

      ::s/bang-equal
      (not= le re)

      ::s/equal-equal
      (= le re)

      ::s/greater-equal
      (>= le re)

      ::s/less-equal
      (<= le re)

      ::s/greater
      (> le re)

      ::s/less
      (< le re))))

(defmethod eval-expr UnaryExpression
  [{:keys [oper right] :as _expr}]
  (case (:type oper)
    ::s/minus
    (- (eval-expr right))

    ::s/bang
    (not (eval-expr right))))

(defmethod eval-expr AssignmentExpression
  [{:keys [identifier expr]}]
  (let [var-name (:lexeme identifier)]
    (assign-variable! var-name expr)
    (variable->value var-name)))

(defmethod eval-expr LiteralExpression
  [{:keys [val] :as _expr}]
  val)

(defmethod eval-expr LogicalExpression
  [{:keys [oper left right ] :as _expr}]
  (case (:type oper)
    ::s/or
    (let [left-v (eval-expr left)]
      (if (or (nil? left-v) (false? left-v))
        (eval-expr right)
        left-v))

    ::s/and
    (let [left-v (eval-expr left)]
      (if (or (nil? left-v) (false? left-v))
        left-v
        (eval-expr right)))))

(defmethod eval-expr VariableExpression
  [expr]
  (let [lexeme (get-in expr [:identifier :lexeme])]
    (if (seq lexeme)
      (variable->value lexeme)
      (throw (ex-info
              "missing identifier for variable expression"
              {:runtime-error true})))))

(defmulti eval-stmt class)

(defmethod eval-stmt Block
  [{:keys [declarations]}]
  (binding [*state* (cons (atom {}) *state*)]
    (run! eval-stmt declarations)))

(defmethod eval-stmt ExpressionStatement
  [{:keys [expr]}]
  (eval-expr expr)
  nil)

(defmethod eval-stmt IfStatement
  [{:keys [expr then-stmt else-stmt]}]
  (let [r (eval-expr expr)]
    (if (or (= r nil) (= r false))
      (when else-stmt
        (eval-stmt else-stmt))
      (eval-stmt then-stmt))))

(defmethod eval-stmt WhileStatement
  [{:keys [expr stmt]}]
  (loop [r (eval-expr expr)]
    (when (truthy? r)
      (eval-stmt stmt)
      (recur (eval-expr expr)))))

(defmethod eval-stmt PrintStatement
  [{:keys [expr]}]
  (println (eval-expr expr)))

(defmethod eval-stmt VarStatement
  [{:keys [identifier expr]}]
  (if-let [var-name (:lexeme identifier)]
    (declare-variable! var-name expr)
    (throw (ex-info "missing identifier for var statement"
                    {:runtime-error true}))))

(defn interpret
  "Evaluate statements. Statements is a seq of ASTs.

  Each AST is a lox.statement (PrintStatement, ExpressionStatement, etc.)"
  [statements]
  (binding [*state* (cons (atom {}) *state*)]
    (run! eval-stmt statements)))
