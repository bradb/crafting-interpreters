(ns lox.scanner)

(defrecord Token [type lexeme literal line])

(defn- token
  [type s lexeme line]
  (Token. type s lexeme line))

(defn- error
  [c line]
  {:message (str "Unrecognised character: " c), :line line})

(defn- char->token
  [c line]
  (case c
    \(
    (token ::left_paren (str c) nil line)

    \)
    (token ::right_paren (str c) nil line)

    \{
    (token ::left_brace (str c) nil line)

    \}
    (token ::right_brace (str c) nil line)

    \,
    (token ::comma (str c) nil line)

    \.
    (token ::dot (str c) nil line)

    \;
    (token ::semicolon (str c) nil line)

    \-
    (token ::minus (str c) nil line)

    \+
    (token ::plus (str c) nil line)

    \*
    (token ::star (str c) nil line)

    nil))

(defn scan
  "Return a coll of tokens from a string of lox code."
  [s]
  (when (seq s)
    (loop [chars (vec s)
           line 1
           tokens []
           errors []]
      (if-let [c (first chars)]
        (let [token (char->token c line)]
          (if token
            (recur (rest chars)
                   line
                   (conj tokens token)
                   errors)
            (recur (rest chars)
                   line
                   (conj tokens token)
                   (error (str c) line))))
        tokens))))
