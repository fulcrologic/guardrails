(ns com.fulcrologic.guardrails.ghostwheel-port
  (:require
    [com.fulcrologic.guardrails.core :refer [>defn =>]]
   ))

(comment
  (macroexpand-1 '(>defn f [i] [int? => int?]
                    42))
  )
