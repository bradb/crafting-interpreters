(ns lox.run
  (:require
   [lox.parse :as lp]
   [lox.scanner :as ls]
   [lox.interpret :as li]))

(defn run
  "Evaluate s as lox source code.

  Returns a string result."
  [s]
  (let [{scan-errors :errors, scan-tokens :tokens} (ls/scan s)]
    (if (seq scan-errors)
      (throw (ex-info (str "error parsing input")
                      {:parse-error true, :errors scan-errors}))
      (let [{stmts :statements, parse-errors :errors, remaining-tokens :tokens} (lp/parse scan-tokens)]
        (if (seq parse-errors)
          (throw (ex-info (str "error parsing input")
                          {:parse-error true, :errors parse-errors}))
          (run! li/interpret stmts))))))


