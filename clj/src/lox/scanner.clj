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

(defn- munch-str-literal
  [s line]
  (when (= \" (first s))
    (loop [munched-chars [(first s)]
           unmunched-chars (rest s)]
      (when-let [current-char (first unmunched-chars)]
        (if (= \" current-char)
          {:string (apply str (conj munched-chars current-char))
           :line line
           :unmunched (rest unmunched-chars)}
          (recur (conj munched-chars current-char)
                 (rest unmunched-chars)))))))

(defn- digit?
  [c]
  (when c
    (<= (int \0) (int c) (int \9))))

(defn- munch-number-literal
  [s line]
  (when (digit? (first s))
    (loop [munched-chars [(first s)]
           unmunched-chars (rest s)]
      (let [next-char (first unmunched-chars)]
        (cond
          (digit? next-char)
          (recur (conj munched-chars next-char) (rest unmunched-chars))

          (= \. next-char)
          (if (digit? (second unmunched-chars))
            (recur (conj munched-chars next-char (second unmunched-chars))
                   (drop 2 unmunched-chars))
            {:error "Numbers cannot begin or end with a dot.", :s (drop 2 unmunched-chars), :line line})

          :else
          (let [lexeme (apply str munched-chars)]
            {:s unmunched-chars
             :line line
             :token (token ::number lexeme (Float/parseFloat lexeme) line)}))))))

(comment
  (munch-number-literal "42." 1))

(defn- trim-first-last
  [s]
  (let [len (.length s)]
    (.subSequence s 1 (- len 1))))

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

      (= c \")
      (let [{:keys [string line unmunched]} (munch-str-literal s line)]
        (when (seq string)
          {:s unmunched, :line line, :token (token ::string string (trim-first-last string) line)}))

      (digit? c)
      (munch-number-literal s line)

      :else
      {:error (str "Unexpected character '" c "'"), :s unscanned-str, :line line}
      )))

(defn scan
  "Return a mapped containing the following keys:
  :tokens - the valid tokens extracted from s
  :errors - a list of maps with keys :message and :line"
  [s]
  (when (seq s)
    (loop [chars (vec s)
           current-line 1
           tokens []
           errors []]
      (if (seq chars)
        (let [{:keys [error s line token]} (next-token chars current-line)
              current-line (or line current-line)]
          (if (seq error)
            (recur s
                   current-line
                   tokens
                   (conj errors {:message error, :line line}))
            (recur s
                   current-line
                   (conj tokens token)
                   errors)))
        {:tokens tokens, :errors errors}))))
