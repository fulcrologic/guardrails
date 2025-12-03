(ns com.fulcrologic.guardrails.malli.core
  #?(:cljs (:require-macros com.fulcrologic.guardrails.malli.core))
  (:require
    [com.fulcrologic.guardrails.malli.registry :as gr.reg :refer [register!]]
    #?@(:clj [[clojure.spec.alpha :as s]
              [com.fulcrologic.guardrails.config :as gr.cfg]
              [com.fulcrologic.guardrails.impl.pro :as gr.pro]
              [com.fulcrologic.guardrails.utils :as utils :refer [cljs-env? clj->cljs]]])
    [clojure.string :as str]
    [com.fulcrologic.guardrails.core :as gr.core]
    [malli.core :as m]
    [malli.dev.pretty :as mp]
    [com.fulcrologic.guardrails.malli.formatting :as v]
    [malli.error :as me]
    [malli.registry]))

;;; Operators ;;;

(def => :ret)
(def | :st)
(def <- :gen)

;;; Syntax specs

(comment
  #?(:clj
     (def spec-elem-malli-only
       (s/or
         :pred-sym (s/and symbol?
                     (complement #{'| '=>})
                     #(str/ends-with? (str %) "?"))
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
                   #(str/ends-with? (str %) "?"))
       :gspec (s/or :nilable-gspec ::gr.core/nilable-gspec :gspec ::gr.core/gspec)
       :spec-key qualified-keyword?
       :malli-key (s/and simple-keyword? (complement #{:st :gen :ret}))
       :malli-sym (s/and symbol? (complement #{'| '=> '<-}))
       :malli-vec (s/and vector? (comp simple-keyword? first))
       :fun ::gr.core/pred-fn
       :list seq?)))

;;; Schema definition helpers

#?(:clj
   (defmacro ? [& forms]
     (cond-> `[:maybe ~@forms]
       (cljs-env? &env) clj->cljs)))

(def ^:dynamic *coll-check-limit* #?(:clj s/*coll-check-limit* :cljs 4))

#?(:clj
   (defmacro every
     ([schema]
      `(every ~schema *coll-check-limit*))
     ([schema sample-limit]
      (cond-> `[:and coll? [:fn #(m/validate [:sequential ~schema] (take ~sample-limit %))]]
        (cljs-env? &env) clj->cljs))))

;;; Main macros

#?(:clj
   (defmacro >def [k v]
     (let [cfg  (gr.cfg/get-env-config)
           mode (gr.cfg/mode cfg)]
       ;; NOTE: Possibly manual override to always include them?
       (when (and cfg (#{:runtime :all} mode))
         `(register! ~k ~v)))))

(defn validate [schema value] (m/validate schema value {:registry gr.reg/registry}))
(defn explain [schema value] (m/explain schema value {:registry gr.reg/registry}))

(defn -block [text body printer]
  [:group (v/-text text printer) [:align 2 body]])

(defn -exception-doc [e printer]
  (let [{:keys [body] :or {title (:title printer)}} (v/format e printer)]
    [:group body]))

(defn reporter
  ([] (reporter (mp/-printer)))
  ([printer]
   (fn [type data]
     (-> (ex-info (str type) {:type type :data data})
       (-exception-doc printer)
       (v/-print-doc printer)
       #?(:cljs (-> with-out-str println))))))

(defmethod v/-format ::explain [_ {:keys [schema] :as explanation} printer]
  {:body
   [:group
    (-block "Schema: " (v/-visit schema printer) printer)]})

(defn verbose-humanize-schema [data opts]
  (with-out-str
    ((mp/prettifier
       ::m/explain
       "Validation Error"
       #(me/with-error-messages data)
       (merge opts {:registry gr.reg/registry})))))

(defn humanize-schema [{:keys [schema] :as explain-data} {:guardrails/keys [fqnm compact? args?] :as opts}]
  (if compact?
    (let [v     (me/error-value explain-data {::me/mask-valid-values '_})
          lines (if args?
                  [(pr-str (apply list (symbol fqnm) v))
                   (str "Arg Errors: " (mapv (fn [v] (if (some? v) v '_)) (me/humanize explain-data)))]
                  [(str (pr-str (list (symbol fqnm) '...)) " => " v)
                   (str "Return value " (first (me/humanize explain-data)))])
          lines (into lines
                  (->
                    (with-out-str
                      ((mp/prettifier ::explain ""
                         #(me/with-error-messages explain-data)
                         (merge opts {::mp/actor reporter
                                      :registry  gr.reg/registry}))))
                    (str/split-lines)))]
      (str "  " (str/join "\n  " lines)))
    (verbose-humanize-schema explain-data opts)))

#?(:clj
   (do
     (defmacro >defn
       "Like defn, but requires a (nilable) gspec definition and generates
       additional Malli function schema metadata and validation code."
       {:arglists '([name doc-string? attr-map? [params*] gspec prepost-map? body?]
                    [name doc-string? attr-map? ([params*] gspec prepost-map? body?) + attr-map?])}
       [& forms]
       (let [env (merge &env `{:guardrails/validate-fn validate
                               :guardrails/explain-fn  explain
                               :guardrails/humanize-fn humanize-schema})]
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
       (let [env (merge &env `{:guardrails/validate-fn validate
                               :guardrails/explain-fn  explain
                               :guardrails/humanize-fn humanize-schema})]
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
              ~(when (#{:pro :all} (gr.cfg/mode cfg))
                 (gr.pro/>fdef-impl env forms))
              ~(cond-> (remove nil? (gr.core/generate-fdef env forms))
                 (cljs-env? &env) clj->cljs)))))

     (s/fdef >fdef :args ::gr.core/>fdef-args)))
