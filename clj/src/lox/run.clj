(ns lox.run
  (:require
   [lox.parse :as lp]
   [lox.scanner :as ls]))

(defn run
  "Evaluate s as lox source code.

  Returns a string result."
  [s]
  (-> s
      ls/scan
      :tokens
      lp/parse
      li/interpret))
