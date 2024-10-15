(ns lox.scanner)

(defrecord Token [type lexeme literal line])

(def ^:private single-char-lexemes
  {\( ::left-paren
   \) ::right-paren
   \{ ::left-brace
   \} ::right-brace
   \, ::comma
   \. ::dot
   \; ::semicolon
   \- ::minus
   \+ ::plus
   \* ::star})


(defn- token
  [type s lexeme line]
  (Token. type s lexeme line))

(defn- error
  [c line]
  {:message (str "Unrecognised character: " c), :line line})

(defn- next-token
  [s line]
  (let [c (first s)
        unscanned-str (rest s)]
    (cond
      (contains? single-char-lexemes c)
      {:s unscanned-str, :line line, :token (token (get single-char-lexemes c) (str c) nil line)}

      (= c \!)
      (if (= \= (first unscanned-str))
        {:s (rest unscanned-str), :line line, :token (token ::bang-equal "!=" nil line)}
        {:s unscanned-str, :line line, :token (token ::bang (str c) nil line)})

      (= c \=)
      (if (= \= (first unscanned-str))
        {:s (rest unscanned-str), :line line, :token (token ::equal-equal "==" nil line)}
        {:s unscanned-str, :line line, :token (token ::equal (str c) nil line)})

      (= c \<)
      (if (= \= (first unscanned-str))
        {:s (rest unscanned-str), :line line, :token (token ::less-equal "<=" nil line)}
        {:s unscanned-str, :line line, :token (token ::less (str c) nil line)})

      (= c \>)
      (if (= \= (first unscanned-str))
        {:s (rest unscanned-str), :line line, :token (token ::greater-equal ">=" nil line)}
        {:s unscanned-str, :line line, :token (token ::greater (str c) nil line)})

      :else
      nil
      )))

(defn scan
  "Return a coll of tokens from a string of lox code."
  [s]
  (when (seq s)
    (loop [chars (vec s)
           line 1
           tokens []
           errors []]
      (if (seq chars)
        (let [{:keys [s line token]} (next-token chars line)]
          (if token
            (recur s
                   line
                   (conj tokens token)
                   errors)
            (recur s
                   line
                   (conj tokens token)
                   (error (first chars) line))))
        tokens))))
