(ns com.fulcrologic.guardrails.core-spec
  (:require
    [com.fulcrologic.guardrails.config :as config]
    [com.fulcrologic.guardrails.core :refer [>defn =>]]
    [clojure.spec.alpha :as s]
    [clojure.test :refer [deftest is]]))

#?(:clj
   (deftest can-load-env-config
     (is (= {:defn-macro nil
             :expound    {:show-valid-values? true
                          :print-specs?       true}} (config/get-env-config false)))))

(comment
  (>defn f
    [[x y z]]
    [(s/coll-of int?) => int?]
    (+ x y z))
  (f [1 2 3])
  (macroexpand-1 '(>defn f [i] [int? => int?]
                    42))
  )
