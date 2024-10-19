(ns lox.parse
  "Parser for the Lox programming language."
  (:require [lox.scanner :as s])
  (:import [lox.expr GroupingExpr BinaryExpr UnaryExpr LiteralExpr]))

(def primary? #{::s/number
                ::s/true
                ::s/false
                ::s/nil
                ::s/string})

(defn- primary
  [tokens]
  (when-let [pt (first tokens)]
    (when (primary? (:type pt))
      (let [literal (case (:type pt)
                      ::s/true
                      true

                      ::s/false
                      false

                      (:literal pt))]
        {:expr (LiteralExpr. literal), :tokens (rest tokens)}))))

(defn- unary
  ([tokens]
   (unary tokens []))
  ([tokens opers]
   (let [{p :expr, ts :tokens} (primary tokens)]
     (cond
       (seq p)
       (let [expr
             (reduce (fn wrap-opers [right oper]
                       (UnaryExpr. oper right))
                     p
                     (reverse opers))]
         {:expr expr, :tokens ts})

       (#{::s/bang ::s/minus} (:type (first tokens)))
       (unary (rest tokens) (conj opers (first tokens)))

       :else
       nil))))

(defn- factor
  ([tokens]
   (factor tokens []))
  ([tokens rights]
   (let [{u :expr, ts :tokens} (unary tokens)]
     (if (seq u)
       (if (#{::s/star ::s/slash} (:type (first ts)))
         (recur (rest ts) (concat [(BinaryExpr. (first ts) u nil)] rights))
         (let [expr (reduce (fn [ex right-expr]
                              (assoc right-expr :right ex))
                            u
                            rights)]
           {:expr expr, :tokens ts}))
       {:expr nil, :tokens tokens}))))

(defn- expression
  [tokens]
  (factor tokens))

(defn parse
  "Map a coll of tokens to an AST.

  The root of the AST is an expr from lox.expr, e.g. GroupingExpr, BinaryExpr, etc."
  [tokens]
  (expression tokens))


