;; TODO: refactor:
;; - use consume for semicolons
;; - fix kw namespacing in scanner/parser
;; - reduce nested conditionals

(ns lox.parse
  "Parser for the Lox programming language."
  (:require [lox.scanner :as s])
  (:import [lox.statement Block VarStatement AssignmentExpression
            VariableExpression PrintStatement ExpressionStatement
            BinaryExpression UnaryExpression LiteralExpression
            GroupingExpression IfStatement LogicalExpression
            WhileStatement]))

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

(defn- consume!
  [tokens expected-type msg]
  (if (= expected-type (:type (first tokens)))
    (rest tokens)
    (throw (ex-info msg {:parse-error true, :tokens (drop-current-statement tokens)}))))

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

(defn- logic-and
  ([tokens]
   (logic-and tokens []))
  ([tokens eqlty-exprs]
   (if-let [eq-expr (equality tokens)]
     (let [tks (:tokens eq-expr)]
       (if (= ::s/and (:type (first tks)))
         (recur (rest tks) (conj eqlty-exprs (:expr eq-expr)))
         (let [expr (reduce (fn [acc x]
                              (LogicalExpression. (s/token ::s/and "and" nil 1) x acc))
                            (:expr eq-expr)
                            eqlty-exprs)]
           {:expr expr, :tokens tks})))
     (when (seq eqlty-exprs)
         (let [expr (reduce (fn [acc x]
                              (LogicalExpression. (s/token ::s/and "and" nil 1) x acc))
                            eqlty-exprs)]
           {:expr expr, :tokens tokens})))))


(defn- logic-or
  ([tokens]
   (logic-or tokens []))
  ([tokens and-exprs]
   (if-let [and-expr (logic-and tokens)]
     (let [tks (:tokens and-expr)]
       (if (= ::s/or (:type (first tks)))
         (recur (rest tks) (conj and-exprs (:expr and-expr)))
         (let [expr (reduce (fn [acc x]
                              (LogicalExpression. (s/token ::s/or "or" nil 1) x acc))
                            (:expr and-expr)
                            and-exprs)]
           {:expr expr, :tokens tks})))
     (when (seq and-exprs)
       (let [expr (reduce
                   (fn [acc x]
                     (LogicalExpression. (s/token ::s/or "or" nil 1) x acc))
                   and-exprs)]
         {:expr expr, :tokens tokens})))))

(defn- assignment
  ([tokens]
   (assignment tokens []))
  ([tokens assign-exprs]
   (cond
     (= [::s/identifier ::s/equal] (->> tokens (take 2) (map :type)))
     (recur (drop 2 tokens) (conj assign-exprs (first tokens)))

     (seq (logic-or tokens))
     (let [or-expr (logic-or tokens)
           assign-expr (reduce (fn [acc x]
                                 (AssignmentExpression. x acc))
                               (:expr or-expr)
                               (rseq assign-exprs))]
       {:expr assign-expr, :tokens (:tokens or-expr)})

     :else
     (when (seq assign-exprs)
         (let [expr (reduce (fn [acc x]
                              (AssignmentExpression. x acc))
                            (rseq assign-exprs))]
           {:expr expr, :tokens tokens})))))

(defn- expression
  [tokens]
  (assignment tokens))

(declare declaration statement) ;; because why not

(defn- parse-if-stmt
  [tokens]
  {:pre [(= ::s/if (:type (first tokens)))]}
  (let [tokens (consume! (rest tokens) ::s/left-paren "expected '(' for if condition")
        {if-expr :expr, rest-tks :tokens} (expression tokens)]
    (if (seq if-expr)
      (let [rest-tks (consume! rest-tks ::s/right-paren "expected ')' after if condition")
            {then-stmt :statement, rest-tks :tokens} (statement rest-tks)]
        (if (seq then-stmt)
          (if (= ::s/else (:type (first rest-tks)))
            (let [{else-stmt :statement, rest-tks :tokens} (statement (rest rest-tks))]
              (if (seq else-stmt)
                {:statement (IfStatement. if-expr then-stmt else-stmt), :tokens rest-tks}
                (throw (ex-info "missing else statement", {:parse-error true, :tokens (drop-current-statement rest-tks)}))))
            {:statement (IfStatement. if-expr then-stmt nil), :tokens rest-tks})
          (throw (ex-info "missing then statement for if statement"
                          {:parse-error true, :tokens (drop-current-statement rest-tks)}))))
      (throw (ex-info "expected condition for if statement, got " (:type (first (rest tokens)))
                      {:parse-error true, :tokens (drop-current-statement tokens)})) )))

(defn- parse-while-stmt
  [tokens]
  (let [tokens (-> tokens
                   (consume! ::s/while "expected while statement")
                   (consume! ::s/left-paren "expected '(' after while" ))]
    (if-let [expr (expression tokens)]
      (let [tokens (consume! (:tokens expr) ::s/right-paren "missing closing ')' after while expression")]
        (if-let [stmt (statement tokens)]
          {:statement (WhileStatement. (:expr expr) (:statement stmt)), :tokens (:tokens stmt)}
          (throw (ex-info "missing statement for while statement" {:parse-error true, :tokens (drop-current-statement tokens)}))   ))
      (throw (ex-info "missing expression for while statement" {:parse-error true, :tokens (drop-current-statement tokens)})))))

(defn- parse-for-stmt
  [tokens]
  (let [expr-stmt (fn [tks]
                    (when-let [expr (expression tks)]
                      (let [tokens (consume! (:tokens expr) ::s/semicolon "expected ';' after for initialiser")]
                        {:statement expr, :tokens tokens})))
        tokens (-> tokens
                   (consume! ::s/for "expected for statement")
                   (consume! ::s/left-paren "expected '(' after for"))
        {init :statement, tks :tokens} (or (declaration tokens) (expr-stmt tokens))
        {cond-expr :expr, tks :tokens} (expression tks)
        tokens (consume! (or tks tokens) ::s/semicolon "expected ';' after for conditional")
        {inc-expr :expr, tks :tokens} (expression tokens)
        tokens (consume! (or tks tokens) ::s/right-paren "expected ')' after for increment")
        {stmt :statement, tks :tokens} (statement tokens)]
    (when (not (seq stmt))
      (throw (ex-info "missing statement body for while" {:parse-error true})))
    (let [statements (if (seq init)
                       [init]
                       [])
          while-cond (if (seq cond-expr)
                       cond-expr
                       (LiteralExpression. true))
          while-stmt (if (seq inc-expr)
                       (let [inc-expr-stmt (ExpressionStatement. inc-expr)]
                         (WhileStatement. while-cond (if (= Block (class stmt))
                                                       (update stmt :declarations conj inc-expr-stmt)
                                                       (Block. [stmt inc-expr-stmt]))))
                       (WhileStatement. while-cond stmt))
          statements (conj statements while-stmt)]
      {:statement (Block. statements), :tokens tks})))

(defn- statement
  [tokens]
  (when (seq tokens)
    (case (:type (first tokens))

      ::s/print
      (let [{expr :expr, rest-tokens :tokens} (expression (rest tokens))]
        (if (seq expr)
          (if (= ::s/semicolon (:type (first rest-tokens)))
            {:statement (PrintStatement. expr), :tokens (rest rest-tokens)}
            (throw (ex-info "missing semicolon after print statement"
                            {:parse-error true, :tokens (drop-current-statement rest-tokens)})))
          (throw (ex-info "missing expression for print statement"
                          {:parse-error true, :tokens (drop-current-statement rest-tokens)}))))

      ::s/if
      (parse-if-stmt tokens)

      ::s/while
      (parse-while-stmt tokens)

      ::s/for
      (parse-for-stmt tokens)

      ::s/left-brace
      (loop [{stmt :statement, tks :tokens} (declaration (rest tokens))
             decls []]
        (case [(nil? stmt) (= ::s/right-brace (:type (first tks)))]
          [true true]
          {:statement (Block. decls), :tokens (rest tks)}

          [true false]
          (throw (ex-info "expected closing brace '}'", {:parse-error true}))

          [false true]
          {:statement (Block. (conj decls stmt)), :tokens (rest tks)}

          [false false]
          (recur (declaration tks) (conj decls stmt))))

      (let [{expr :expr, tks :tokens} (expression tokens)]
        (when-not (seq expr)
          (throw (ex-info "missing expression for statement"
                          {:parse-error true, :tokens (drop-current-statement tks)})))
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
            {:statement (VarStatement. maybe-identifier (LiteralExpression. nil)), :tokens rest-tokens}

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
