(ns user
  (:require
    [com.fulcrologic.guardrails.core :refer [>defn >def => | ?]]
    [clojure.tools.namespace.repl :as tools-ns]
    [clojure.spec.alpha :as s]))

;; >def (and >fdef) can be used to remove specs from production builds. Use them to define
;; specs that you only need in development, and they will not hurt CLJS production build size.
(>def ::thing (s/or :i int? :s string?))

;; When guardrails is disabled this will just be a normal `defn`, and no fspec overhead will
;; appear in cljs builds. When enabled it will check the inputs/outputs and *always* log
;; an error using `expound`, and then *optionally* throw an exception,
(>defn f [i]
  [int? | #(< -10 i 5) => int? | #(> i %)]
  (inc i))

(comment
  (f -2))
