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

(defn- munch-str-literal
  [s line]
  (when (= \" (first s))
    (loop [munched-chars [(first s)]
           unmunched-chars (rest s)]
      (if-let [current-char (first unmunched-chars)]
        (if (= \" current-char)
          {:token (token ::string (apply str (conj munched-chars current-char)) (apply str (rest munched-chars)) line)
           :line line
           :s (rest unmunched-chars)}
          (recur (conj munched-chars current-char)
                 (rest unmunched-chars)))
        {:error (str "Unterminated string: " (apply str munched-chars))
         :line line
         :s unmunched-chars}))))

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

          (and (= \. next-char)
               (digit? (second unmunched-chars)) )
          (recur (conj munched-chars next-char (second unmunched-chars))
                 (drop 2 unmunched-chars))

          :else
          (let [lexeme (apply str munched-chars)]
            {:s unmunched-chars
             :line line
             :token (token ::number lexeme (Float/parseFloat lexeme) line)}))))))

(defn- alpha?
  [c]
  (when c
    (or (<= (int \a) (int c) (int \z))
        (<= (int \A) (int c) (int \Z))
        (= (int \_) (int c)))))

(def ^:private alpha-numeric? (some-fn alpha? digit?))

(defn- munch-identifier
  [s line]
  (when (alpha? (first s))
    (loop [lexeme [(first s)]
           unmunched (rest s)]
      (let [next-char (first unmunched)]
        (if (alpha-numeric? next-char)
          (recur (conj lexeme next-char)
                 (rest unmunched))
          {:s unmunched
           :line line
           :token (token ::identifier (apply str lexeme) nil line)})))))

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
      (munch-str-literal s line)

      (digit? c)
      (munch-number-literal s line)

      (alpha-numeric? c)
      (munch-identifier s line)

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
