(ns com.fulcrologic.guardrails.impl.pro
  (:require
   [com.fulcrologic.guardrails.impl.externs :as gr.externs]
   [com.fulcrologic.guardrails.impl.parser :as gr.parser]
   [com.fulcrologic.guardrails.registry :as gr.reg]
   [com.fulcrologic.guardrails.utils :as utils]))

(defn compiling-cljs? [env]
  (and (:ns env) (utils/compiling-cljs?)))

(defn env->NS [env]
  (if (compiling-cljs? env)
    (-> env :ns :name name)
    (-> *ns* ns-name name)))

(defn env->namespace-meta [env]
  (if (compiling-cljs? env)
    (-> env :ns :meta)
    (-> *ns* meta)))

(defn defn-registration
  "Returns the form that records the `>defn` `body` in the function registry. `opts` may contain
   `:guardrails/malli?` and `:guardrails/fn-ref` (a form to register as the function's reference instead of its name)."
  [env body {:guardrails/keys [fn-ref] :as opts}]
  (let [externs     (gr.externs/extern-symbols env body)
        NS          (env->NS env)
        {:guardrails/keys [spec-system]
         :or              {spec-system :org.clojure/spec1}} (env->namespace-meta env)
        spec-system (if (:guardrails/malli? opts) :malli spec-system)
        parsed-defn (cond-> (assoc (gr.parser/parse-defn body externs)
                                   ::gr.reg/spec-system spec-system)
                      fn-ref (assoc ::gr.reg/fn-ref fn-ref))]
    `(gr.externs/record-defn! ~NS ~parsed-defn ~externs)))

(defn >defn-impl [env body opts]
  `(do ~(defn-registration env body opts)
       (var ~(first body))))

(defn >fdef-impl [env body]
  (let [externs     (gr.externs/extern-symbols env body)
        parsed-fdef (gr.parser/parse-fdef body externs)]
    `(gr.externs/record-fdef! ~parsed-fdef)))

(defn >malli-fdef-impl [env body]
  (let [externs     (gr.externs/extern-symbols env body)
        parsed-fdef (gr.parser/parse-fdef body externs)]
    `(gr.externs/record-malli-fdef! ~parsed-fdef)))

(defn >fspec-impl [env body]
  (let [fspec (gr.parser/parse-fspec body)]
    `(do
       (gr.externs/register-specs! ~fspec)
       ~(gr.externs/clean-function fspec))))
