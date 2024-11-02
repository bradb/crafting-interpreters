;; TODO: refactor :)

(ns lox.parse
  "Parser for the Lox programming language."
  (:require [lox.scanner :as s])
  (:import [lox.statement VarStatement AssignmentExpression VariableExpression
            PrintStatement ExpressionStatement GroupingExpression
            BinaryExpression UnaryExpression LiteralExpression]))

(def literal? #{::s/number
                ::s/true
                ::s/false
                ::s/nil
                ::s/string})

(declare expression)

(defn- drop-current-statement
  [tokens]
  (let [ts (drop-while #(not= (:type %) ::s/semicolon) tokens)]
    (if (= (:type (first ts)) ::s/semicolon)
      (rest ts)
      ts)))

(defn- primary
  [tokens]
  (when-let [tk (first tokens)]
    (cond
      (= ::s/left-paren (:type tk))
      (let [{rest-tokens :tokens, expr :expr} (expression (rest tokens))]
        (if (seq expr)
          (if (= ::s/right-paren (->> rest-tokens
                                      first
                                      :type))
            {:expr (GroupingExpression. expr), :tokens (rest rest-tokens)}
            (throw (ex-info "missing expected closing ')'", {:parse-error true
                                                             :tokens (drop-current-statement tokens)})))
          (throw (ex-info "missing expression after '('", {:parse-error true
                                                           :tokens (drop-current-statement tokens)}))))

      (literal? (:type tk))
      (let [literal (case (:type tk)
                      ::s/true
                      true

                      ::s/false
                      false

                      (:literal tk))]
        {:expr (LiteralExpression. literal), :tokens (rest tokens)})

      (= ::s/identifier (:type tk))
      {:expr (VariableExpression. tk), :tokens (rest tokens)})))

(defn- unary
  ([tokens]
   (unary tokens []))
  ([tokens opers]
   (let [{p :expr, ts :tokens} (primary tokens)]
     (cond
       (seq p)
       (let [expr
             (reduce (fn wrap-opers [right oper]
                       (UnaryExpression. oper right))
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
  ([tokens chunks]
   (let [{ex :expr, ts :tokens} (unary tokens)]
     (if (seq ex)
       (if (#{::s/slash ::s/star} (:type (first ts)))
         (recur (rest ts) (cons [ex (first ts)] chunks))
         (let [expr (reduce
                     (fn [ex [u op]]
                       (BinaryExpression. op u ex))
                     ex
                     chunks)]
           {:expr expr, :tokens ts}))
       (let [[_ op] (last (seq chunks))]
         (if (seq op)
           (throw (ex-info (str "expected expression after '" (:lexeme op) "'")
                           {:parse-error true, :tokens (drop-current-statement tokens)}))
           {:expr ex, :tokens ts}))))))

(defn- term
  ([tokens]
   (term tokens []))
  ([tokens rights]
   (let [{fc :expr, ts :tokens} (factor tokens)]
     (if (seq fc)
       (if (#{::s/plus ::s/minus} (:type (first ts)))
         (recur (rest ts) (conj rights (BinaryExpression. (first ts) fc nil)))
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
         (recur (rest ts) (conj rights (BinaryExpression. (first ts) tm nil)))
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
         (recur (rest ts) (conj rights (BinaryExpression. (first ts) cp nil)))
         (let [expr (reduce (fn [ex right-expr]
                              (assoc right-expr :right ex))
                            cp
                            (rseq rights))]
           {:expr expr, :tokens ts}))
       {:expr nil, :tokens tokens}))))

(defn- assignment
  ([tokens]
   (assignment tokens []))
  ([tokens rights]
   (if (= [::s/identifier ::s/equal] (->> tokens
                                          (take 2)
                                          (map :type)))
     (recur (drop 2 tokens) (conj rights (first tokens)))
     (let [{expr :expr, tks :tokens} (equality tokens)
           assign-expr (reduce (fn assigns [acc x]
                                 (AssignmentExpression. x acc)) expr (rseq rights))]
       {:expr assign-expr, :tokens tks}))))

(defn- expression
  [tokens]
  (assignment tokens))

(defn- statement
  [tokens]
  (when (seq tokens)
    (if (= ::s/print (:type (first tokens)))
      (let [{expr :expr, rest-tokens :tokens} (expression (rest tokens))]
        (if (seq expr)
          (if (= ::s/semicolon (:type (first rest-tokens)))
            {:statement (PrintStatement. expr), :tokens (rest rest-tokens)}
            (throw (ex-info "missing semicolon after print statement"
                            {:parse-error true, :tokens (drop-current-statement rest-tokens)})))
          (throw (ex-info "missing expression for print statement"
                          {:parse-error true, :tokens (drop-current-statement rest-tokens)}))))
      (let [{expr :expr, tks :tokens} (expression tokens)]
        (if (= ::s/semicolon (:type (first tks)))
          {:statement (ExpressionStatement. expr), :tokens (rest tks)}
          (throw (ex-info "missing semicolon after expression"
                          {:parse-error true, :tokens (drop-current-statement tks)})))))))

(defn- declaration
  [tokens]
  (if (= ::s/var (:type (first tokens)))
    (let [[maybe-identifier maybe-oper & rest-tokens] (rest tokens)]
      (case (mapv :type [maybe-identifier maybe-oper])
            [::s/identifier ::s/equal]
            (let [{expr :expr, tks :tokens} (expression rest-tokens)]
              (if (= ::s/semicolon (:type (first tks)))
                {:statement (VarStatement. maybe-identifier expr), :tokens (rest tks)}
                (throw (ex-info "missing semicolon after expression"
                                {:parse-error true, :tokens (drop-current-statement tks)}))))

            [::s/identifier ::s/semicolon]
            {:statement (VarStatement. maybe-identifier nil), :tokens rest-tokens}

            (throw (ex-info "var declaration must be of the form 'var IDENTIFIER;' or 'var IDENTIFIER = EXPR;'"
                            {:parse-error true, :tokens (drop-current-statement rest-tokens)}))))
    (statement tokens)))

(defn parse
  "Map a coll of tokens to coll of statements. Returns a map with the following keys:

  :statements - a coll of statements (PrintStatement, ExpressionStatement, etc.)
  :errors - a coll of parsing errors, or nil"
  [tokens]
  (loop [statements []
         errors []
         tokens tokens]
    (if (seq tokens)
      (let [{stmt :statement, errs :errors, tks :tokens} (declaration tokens)]
        (recur (conj statements stmt) (concat errors errs) tks))
      {:statements statements, :errors errors})))
