(ns lox.interpret
  (:require [lox.scanner :as s])
  (:import [lox.statement AssignmentExpression PrintStatement ExpressionStatement
            VarStatement UnaryExpression GroupingExpression BinaryExpression
            Block VariableExpression LiteralExpression]))

(defrecord Scope [outer sym->val])

(def ^:private ^:dynamic *state* (atom nil))

(declare eval-expr)

(defn- declare-variable!
  [k expr]
  (swap! *state* assoc-in [:sym->val k] (eval-expr expr)))

(comment
  (binding [*state* (atom (map->Scope {:outer nil, :sym->val {}}))]
    (declare-variable! "n" nil)
    @*state*))

(defn- assign-variable!
  [k expr]
  (swap! *state*
         (fn [st]
           (loop [{:keys [sym->val outer]} st
                  ks []]
             (if (nil? sym->val)
               (throw (ex-info (str "attempt to assign to undeclared variable '" k "'")
                               {:runtime-error true}))
               (if (contains? sym->val k)
                 (assoc-in st (conj ks :sym->val k) (eval-expr expr))
                 (recur outer (conj ks :outer))))))))

(defn- variable->value
  [k]
  (loop [{:keys [sym->val outer]} @*state*]
    (if (nil? sym->val)
      (throw (ex-info (str "reference to undeclared variable '" k "'")
                      {:runtime-error true}))
      (if (contains? sym->val k)
        (get sym->val k)
        (recur outer)))))

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

(defmethod eval-expr VariableExpression
  [expr]
  (let [lexeme (get-in expr [:identifier :lexeme])]
    (if (seq lexeme)
      (variable->value lexeme)
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

(defmethod eval-stmt Block
  [{:keys [declarations]}]
  (binding [*state* (atom (map->Scope {:sym->val {}, :outer @*state*}))]
    (run! eval-stmt declarations)))

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
  (binding [*state* (atom (map->Scope {:outer nil, :sym->val {}}))]
    (run! eval-stmt statements)))
