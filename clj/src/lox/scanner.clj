(ns lox.scanner)

(defrecord Token [type lexeme literal line])

(defn- token
  [type s lexeme line]
  (Token. type s lexeme line))

(defn- error
  [c line]
  {:message (str "Unrecognised character: " c), :line line})

(defn- next-token
  [s line]
  (let [c (first s)
        rem (rest s)

        next-tkn
        (case c
          \(
          (token ::left-paren (str c) nil line)

          \)
          (token ::right-paren (str c) nil line)

          \{
          (token ::left-brace (str c) nil line)

          \}
          (token ::right-brace (str c) nil line)

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

          \!
          (token ::bang (str c) nil line)

          \=
          (token ::equal (str c) nil line)

          \<
          (token ::less (str c) nil line)

          \>
          (token ::greater (str c) nil line)

          nil)]
    {:s rem, :token next-tkn}))

(defn scan
  "Return a coll of tokens from a string of lox code."
  [s]
  (when (seq s)
    (loop [chars (vec s)
           line 1
           tokens []
           errors []]
      (if (seq chars)
        (let [{:keys [s token]} (next-token chars line)]
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
