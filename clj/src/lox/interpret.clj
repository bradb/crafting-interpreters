(ns lox.interpret
  (:require [lox.scanner :as s])
  (:import [lox.statement AssignmentExpression PrintStatement ExpressionStatement
            VarStatement UnaryExpression GroupingExpression BinaryExpression
            VariableExpression LiteralExpression]))

(def ^:private ^:dynamic *state* (atom {}))

(defmulti eval-expr class)

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
  (let [var-name (:lexeme identifier)
        m (swap! *state* (fn [m]
                         (if (contains? m var-name)
                           (assoc m var-name (eval-expr expr))
                           (throw (ex-info (str "attempt to assign to undeclared variable '"
                                                var-name
                                                "'")
                                           {:runtime-error true})))))]
    (get m var-name)))

(defmethod eval-expr LiteralExpression
  [{:keys [val] :as _expr}]
  val)

(defmethod eval-expr VariableExpression
  [expr]
  (let [lexeme (get-in expr [:identifier :lexeme])]
    (if (seq lexeme)
      (let [state @*state*]
        (if (contains? state lexeme)
          (get state lexeme)
          (throw (ex-info
                  (str "'" lexeme "' is not defined")
                  {:runtime-error true}))))
      (throw (ex-info
              "missing identifier for variable expression"
              {:runtime-error true})))))

(defmulti eval-stmt class)

(defmethod eval-stmt PrintStatement
  [{:keys [expr]}]
  (println (eval-expr expr)))

(defmethod eval-stmt ExpressionStatement
  [{:keys [expr]}]
  (eval-expr expr)
  nil)

(defmethod eval-stmt VarStatement
  [{:keys [identifier expr]}]
  (if-let [var-name (:lexeme identifier)]
    (swap! *state* assoc var-name (eval-expr expr))
    (throw (ex-info "missing identifier for var statement"
                    {:runtime-error true}))))

(defn interpret
  "Evaluate the Lox statement contained in `ast`.

  `ast` a lox.statement (PrintStatement, ExpressionStatement, etc.)"
  [ast]
  (eval-stmt ast))
