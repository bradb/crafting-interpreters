;; TODO: refactor :)

(ns lox.parse
  "Parser for the Lox programming language."
  (:require [lox.scanner :as s])
  (:import [lox.expr GroupingExpr BinaryExpr UnaryExpr LiteralExpr]))

(def literal? #{::s/number
                ::s/true
                ::s/false
                ::s/nil
                ::s/string})

(declare expression)

(defn- drop-current-statement
  [tokens]
  nil)

(comment
  (primary (:tokens (s/scan "(3"))))

(defn- primary
  [tokens]
  (when-let [pt (first tokens)]
    (cond
      (= ::s/left-paren (:type pt))
      (let [{rest-tokens :tokens, expr :expr} (expression (rest tokens))]
        (if (seq expr)
          (if (= ::s/right-paren (->> rest-tokens
                                      first
                                      :type))
            {:expr (GroupingExpr. expr), :tokens (rest rest-tokens)}
            (throw (ex-info "missing expected closing ')'", {:parse-error true
                                                             :tokens (drop-current-statement tokens)})))
          (throw (ex-info "missing expression after '('", {:parse-error true
                                                           :tokens (drop-current-statement tokens)}))))

      (literal? (:type pt))
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

(defn- discard-current-statement
  [tokens]
  (let [ts (drop-while #(not= (:type %) ::s/semicolon) tokens)]
    (if (= (:type (first ts)) ::s/semicolon)
      (rest ts)
      ts)))

(defn- factor
  ([tokens]
   (factor tokens []))
  ([tokens chunks]
   (let [{ex :expr, ts :tokens} (unary tokens)]
     (if (seq ex)
       (if (#{::s/slash ::s/star} (:type (first ts)))
         (recur (rest ts) (cons [ex (first ts)] chunks))
         (let [expr (reduce
                     (fn [ex [u op]]
                       (BinaryExpr. op u ex))
                     ex
                     chunks)]
           {:expr expr, :tokens ts}))
       (let [[_ op] (last (seq chunks))]
         (if (seq op)
           (throw (ex-info (str "expected expression after '" (:lexeme op) "'")
                           {:parse-error true, :tokens (discard-current-statement tokens)}))
           {:expr ex, :tokens ts}))))))

(defn- term
  ([tokens]
   (term tokens []))
  ([tokens rights]
   (let [{fc :expr, ts :tokens} (factor tokens)]
     (if (seq fc)
       (if (#{::s/plus ::s/minus} (:type (first ts)))
         (recur (rest ts) (conj rights (BinaryExpr. (first ts) fc nil)))
         (let [expr (reduce (fn [ex right-expr]
                              (assoc right-expr :right ex))
                            fc
                            (rseq rights))]
           {:expr expr, :tokens ts}))
       {:expr nil, :tokens tokens}))))

(defn- comparison
  ([tokens]
   (comparison tokens []))
  ([tokens rights]
   (let [{tm :expr, ts :tokens} (term tokens)]
     (if (seq tm)
       (if (#{::s/greater ::s/greater-equal ::s/less ::s/less-equal} (:type (first ts)))
         (recur (rest ts) (conj rights (BinaryExpr. (first ts) tm nil)))
         (let [expr (reduce (fn [ex right-expr]
                              (assoc right-expr :right ex))
                            tm
                            (rseq rights))]
           {:expr expr, :tokens ts}))
       {:expr nil, :tokens tokens}))))

(defn- equality
  ([tokens]
   (equality tokens []))
  ([tokens rights]
   (let [{cp :expr, ts :tokens} (comparison tokens)]
     (if (seq cp)
       (if (#{::s/bang-equal ::s/equal-equal} (:type (first ts)))
         (recur (rest ts) (conj rights (BinaryExpr. (first ts) cp nil)))
         (let [expr (reduce (fn [ex right-expr]
                              (assoc right-expr :right ex))
                            cp
                            (rseq rights))]
           {:expr expr, :tokens ts}))
       {:expr nil, :tokens tokens}))))

(defn- expression
  [tokens]
  (try
    (equality tokens)
    (catch Exception e
      (let [{:keys [parse-error tokens]} (ex-data e)]
        (if parse-error
          {:errors [(.getMessage e)], :tokens tokens}
          (throw e))))))

(defn parse
  "Map a coll of tokens to an AST. Returns a lox.expr expression.

  The root of the AST is an expr from lox.expr, e.g. GroupingExpr, BinaryExpr, etc."
  [tokens]
  (expression tokens))


