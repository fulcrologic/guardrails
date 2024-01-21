(ns com.fulcrologic.guardrails.malli.core
  #?(:cljs (:require-macros com.fulcrologic.guardrails.malli.core))
  (:require
    #?@(:clj [[clojure.spec.alpha :as s]
              [clojure.string :as string]
              [com.fulcrologic.guardrails.config :as gr.cfg]
              [com.fulcrologic.guardrails.impl.pro :as gr.pro]
              [com.fulcrologic.guardrails.utils :refer [cljs-env? clj->cljs strip-colors]]])
    [com.fulcrologic.guardrails.core :as gr.core]
    [com.fulcrologic.guardrails.malli.registry :as gr.reg :refer [register!]]
    [malli.core :as m]
    [malli.dev.pretty :as mp]
    [malli.error :as me]
    [malli.registry]))


(def => :ret)
(def | :st)
(def <- :gen)

(comment
  #?(:clj
     (def spec-elem-malli-only
       (s/or
         :pred-sym (s/and symbol?
                     (complement #{'| '=>})
                     ;; REVIEW: should the `?` be a requirement?
                     #(string/ends-with? (str %) "?"))
         :gspec (s/or :nilable-gspec ::gr.core/nilable-gspec :gspec ::gr.core/gspec)
         :spec-key qualified-keyword?
         :malli-key (s/and simple-keyword? (complement #{:st :ret :gen}))
         :malli-sym (s/and symbol? (complement #{'| '=> '<-}))
         :malli-vec (s/and vector? (comp simple-keyword? first))
         :fun ::gr.core/pred-fn))))

#?(:clj
   ;; REVIEW: There doesn't appear to be a straightforward way to properly split
   ;; the leaf specs between spec and malli without duplicating all the root ones,
   ;; so for now we accept that some valid spec elements which would be invalid in
   ;; Malli will pass compile-time macro-syntax checking
   (s/def ::gr.core/spec-elem
     (s/or
       :set set?
       :pred-sym (s/and symbol?
                   (complement #{'| '=>})
                   ;; REVIEW: should the `?` be a requirement?
                   #(string/ends-with? (str %) "?"))
       :gspec (s/or :nilable-gspec ::gr.core/nilable-gspec :gspec ::gr.core/gspec)
       :spec-key qualified-keyword?
       :malli-key (s/and simple-keyword? (complement #{:st :gen :ret}))
       :malli-sym (s/and symbol? (complement #{'| '=> '<-}))
       :malli-vec (s/and vector? (comp simple-keyword? first))
       :fun ::gr.core/pred-fn
       :list seq?)))


(defn humanize-schema [malli-opts explain-data]
  (with-out-str
    ((mp/prettifier
       ::m/explain
       "Validation Error"
       #(me/with-error-messages explain-data)
       malli-opts))))


#?(:clj
   (defmacro ? [& forms]
     (cond-> `[:maybe ~@forms]
       (cljs-env? &env) clj->cljs)))


#?(:clj
   (defmacro >def [k v]
     (let [cfg  (gr.cfg/get-env-config)
           mode (gr.cfg/mode cfg)]
       ;; NOTE: Possibly manual override to always include them?
       (when (and cfg (#{:runtime :all} mode))
         `(register! ~k ~v)))))

(defn default-validate [schema value] (m/validate schema value {:registry gr.reg/registry}))
(defn default-explain [schema value] (malli.core/explain schema value {:registry gr.reg/registry}))
(defn default-humanize [data opts] (humanize-schema (merge opts {:registry gr.reg/registry}) data))

#?(:clj
   (do
     (defmacro >defn
       "Like defn, but requires a (nilable) gspec definition and generates
       additional Malli function schema metadata and validation code."
       {:arglists '([name doc-string? attr-map? [params*] gspec prepost-map? body?]
                    [name doc-string? attr-map? ([params*] gspec prepost-map? body?) + attr-map?])}
       [& forms]
       (let [env (merge &env `{:guardrails/validate-fn default-validate
                               :guardrails/explain-fn  default-explain
                               :guardrails/humanize-fn default-humanize})]
         (gr.core/>defn* env &form forms {:private? false :guardrails/malli? true})))
     (s/fdef >defn :args ::gr.core/>defn-args)))


#?(:clj
   (do
     (defmacro >defn-
       "Like defn-, but requires a (nilable) gspec definition and generates
       additional Malli function schema metadata and validation code."
       {:arglists '([name doc-string? attr-map? [params*] gspec prepost-map? body?]
                    [name doc-string? attr-map? ([params*] gspec prepost-map? body?) + attr-map?])}
       [& forms]
       (let [env (merge &env `{:guardrails/validate-fn default-validate
                               :guardrails/explain-fn  default-explain
                               :guardrails/humanize-fn default-humanize})]
         (gr.core/>defn* env &form forms {:private? true :guardrails/malli? true})))
     (s/fdef >defn- :args ::gr.core/>defn-args)))


#?(:clj
   (do
     (defmacro >fdef
       "Defines a Malli function schema using gspec syntax – pretty much a
       `>defn` without the body. Desugars to `(malli.core/=> ...)."
       {:arglists '([name [params*] gspec]
                    [name ([params*] gspec) +])}
       [& forms]
       (when-let [cfg (gr.cfg/get-env-config)]
         (let [env (assoc &env :guardrails/malli? true)]
           `(do
              ~(when (#{:pro :copilot :all} (gr.cfg/mode cfg))
                 (gr.pro/>fdef-impl env forms))
              ~(cond-> (remove nil? (gr.core/generate-fdef env forms))
                 (cljs-env? &env) clj->cljs)))))

     (s/fdef >fdef :args ::gr.core/>fdef-args)))

