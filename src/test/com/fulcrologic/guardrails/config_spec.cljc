(ns com.fulcrologic.guardrails.config-spec
  (:require
    [com.fulcrologic.guardrails.config :as c]
    [com.fulcrologic.guardrails.malli.core :refer [>defn =>]]
    [fulcro-spec.core :refer [assertions specification component =throws=>]]))

(>defn f
  [x]
  [int? => int?]
  x)

(>defn g
  [x]
  [int? => int?]
  x)

(specification "Exclusions system"
  (let [original @c/current-exclusions]
    (c/clear-exclusions!)

    (c/exclude-checks! 'a.b)
    (c/allow-checks! 'a.b/d)

    (assertions
      "Allows ns and kw selection"
      (c/-excluded? :a.b/d :a.b) => false
      (c/excluded? :a.b/d) => false
      (c/-excluded? :a.b/c :a.b) => true
      (c/excluded? :a.b/c) => true)

    (c/allow-checks! 'a.b)

    (assertions
      "Allows ns and kw selection"
      (c/-excluded? :a.b/d :a.b) => false
      (c/-excluded? :a.b/c :a.b) => false
      (c/excluded? :a.b/d) => false
      (c/excluded? :a.b/c) => false)

    (c/reset-exclusions!)
    (assertions
      "Can be reset back to the originally loaded exclusions"
      @c/current-exclusions => original))

  (component "Controls defn checking"
    (c/clear-exclusions!)
    (assertions
      "Enforces when not excluded"
      (f 1.2) =throws=> #"should be an int")
    (c/exclude-checks! `f)
    (assertions
      "Ignores when excluded"
      (f 1.2) => 1.2
      "Applies at the function level"
      (g 1.2) =throws=> #"should be an int")))
