(ns lox.run
  (:require
   [lox.parse :as lp]
   [lox.scanner :as ls]
   [lox.interpret :as li]
   [clojure.string :as str]))

(defn run
  "Evaluate s as lox source code.

  Returns a string result."
  [s]
  (let [{scan-errors :errors, scan-tokens :tokens} (ls/scan s)]
    (if (seq scan-errors)
      (throw (ex-info (str "error parsing input")
                      {:parse-error true, :errors scan-errors}))
      (let [{ast :expr, parse-errors :errors, remaining-tokens :tokens} (lp/parse scan-tokens)]
        (if (seq parse-errors)
          (throw (ex-info (str "error parsing input")
                          {:parse-error true, :errors parse-errors}))
          (let [result (li/interpret ast)]
            (cond
              (number? result)
              (let [r (.toString result)]
                (if (str/ends-with? r ".0")
                  (subs r 0 (- (.length r) 2))
                  r))

              (nil? result)
              "nil"

              :else
              (str result))))))))


