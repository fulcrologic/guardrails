(ns com.fulcrologic.guardrails.malli.registry
  #?(:cljs (:require-macros com.fulcrologic.guardrails.malli.registry))
  (:require
    [malli.core :as m]
    [malli.registry :as mr]))

(defonce ^{:docstring "The atom that holds the schemas that guardrails will use for validating gspecs. This is a public
atom, and you can choose to manipulate it directly; however, library authors should only add things to this that are
namespaced to the library itself."}
  schema-atom (atom {}))

(defonce ^{:docstring "The Malli registry using by guardrails when validating gspecs. It is a composite registry or malli default registry and mutable registry that
 works from the schema-atom in this ns."}
  registry (mr/composite-registry m/default-registry (mr/mutable-registry schema-atom)))

(defn register!
  "Register the given keyword with the given schema.

   NOTE: Libraries and application code share this registry, thus if your keywords should use namespaces you have
   confidence are distinct. Library authors, in particular, MUST NOT register simple keywords."
  [type ?schema] (swap! schema-atom assoc type ?schema))

(defn merge-schemas!
  "Add the given Malli schemas to the GR registry. All of Malli's default-schemas are merged by default.
   schemas."
  [& schemas]
  (let [combined (apply merge schemas)]
    (swap! schema-atom merge combined)))

(register! :every (m/-collection-schema {:type :every, :pred coll?}))
