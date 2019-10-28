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

(>defn f
  ([a b]
   [int? int? => int?]
   (+ a b))
  ([i]
   [int? => int?]
   (+ 1 i)))

(comment
  (f "22")
  (macroexpand-1 '(>defn f [i] [int? => int?]
                    42))
  )
