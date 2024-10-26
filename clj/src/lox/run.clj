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
  (let [result (-> s
                   ls/scan
                   :tokens
                   lp/parse
                   :expr
                   li/interpret)]
       (cond
         (number? result)
         (let [r (.toString result)]
           (if (str/ends-with? r ".0")
             (subs r 0 (- (.length r) 2))
             r))

         (nil? result)
         "nil"

         :else
         (str result))))


