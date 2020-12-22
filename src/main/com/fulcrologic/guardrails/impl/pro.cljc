(ns com.fulcrologic.guardrails.impl.pro
  (:require
    [com.fulcrologic.guardrails.impl.externs :as gr.externs]
    [com.fulcrologic.guardrails.impl.parser :as gr.parser]
    [com.fulcrologic.guardrails.utils :as utils]))

(defn compiling-cljs? [env]
  (and (:ns env) (utils/compiling-cljs?)))

(defn env->NS [env]
  (if (compiling-cljs? env)
    (-> env :ns :name name)
    (-> *ns* ns-name name)))

(defn >defn-impl [env body opts]
  (let [externs     (gr.externs/extern-symbols env body)
        NS          (env->NS env)
        parsed-defn (gr.parser/parse-defn body externs)]
    `(do (gr.externs/record-defn! ~NS ~parsed-defn ~externs)
         (var ~(first body)))))

(defn >fdef-impl [env body]
  (let [externs     (gr.externs/extern-symbols env body)
        parsed-fdef (gr.parser/parse-fdef body externs)]
    `(gr.externs/record-fdef! ~parsed-fdef)))

(defn >fspec-impl [env body]
  (let [fspec (gr.parser/parse-fspec body)]
    `(do
       (gr.externs/register-specs! ~fspec)
       ~(gr.externs/clean-function fspec))))
