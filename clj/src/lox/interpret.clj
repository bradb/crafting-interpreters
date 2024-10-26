(ns lox.interpret
  (:require [lox.scanner :as s])
  (:import [lox.expr UnaryExpr GroupingExpr BinaryExpr LiteralExpr]))

(defmulti eval-expr class)

(defmethod eval-expr GroupingExpr
  [{:keys [expr] :as _expr}]
  (eval-expr expr))

(defmethod eval-expr BinaryExpr
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

(defmethod eval-expr UnaryExpr
  [{:keys [oper right] :as _expr}]
  (case (:type oper)
    ::s/minus
    (- (eval-expr right))

    ::s/bang
    (not (eval-expr right))))

(defmethod eval-expr LiteralExpr
  [{:keys [val] :as _expr}]
  val)

(defn interpret
  "Recursively evaluate a Lox abstract syntax tree, `ast`.

  The root of the tree is a lox.expr."
  [ast]
  (eval-expr ast))
