;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^:no-doc com.fulcrologic.guardrails.config
  #?(:cljs (:require-macros com.fulcrologic.guardrails.config))
  (:require
    [com.fulcrologic.guardrails.utils :as util]
    [taoensso.timbre :as log]
    #?@(:clj  [[clojure.edn :as edn]]
        :cljs [[cljs.env :as cljs-env]])))

;; This isn't particularly pretty, but it's how we avoid
;; having ClojureScript as a required dependency on Clojure
#?(:clj (try
          (ns-unalias (find-ns 'com.fulcrologic.guardrails.utils) 'cljs-env)
          (require '[cljs.env :as cljs-env])
          (catch Exception _ (require '[com.fulcrologic.guardrails.stubs.cljs-env :as cljs-env]))))

(def default-config
  {;; Generates standard `defn` function definitions
   ;; by default. If you require composability with other
   ;; `defn`-like macros, you can have Ghostwheel desugar to
   ;; them instead by setting the macro name as a string here.
   :defn-macro nil

   ;; Nilable map of Expound configuration options.
   ;; If not nil, the spec printer will be set to
   ;; expound's with the given configuration options.
   :expound    {:show-valid-values? true
                :print-specs?       true}})

(let [*config-cache
      (atom {::timestamp 0
             ::value     nil})

      warned?
      (atom false)

      read-config-file
      (fn []
        #?(:clj  (try
                   (edn/read-string (slurp (or
                                             (System/getProperty "guardrails.config")
                                             "guardrails.edn")))
                   (catch Exception _ nil))
           :cljs nil))

      reload-config
      (fn []
        ;#?(:clj (.println System/err (get @cljs-env/*compiler* :options))) ; DEBUG
        (let [config (let [cljs-compiler-config
                           (when cljs-env/*compiler*
                             (get-in @cljs-env/*compiler* [:options :external-config :guardrails]))]
                       (when #?(:clj (or
                                       cljs-compiler-config
                                       (System/getProperty "guardrails.enabled")) :cljs false)
                         (when-not @warned?
                           (reset! warned? true)
                           (log/warn "GUARDRAILS IS ENABLED. RUNTIME PERFORMANCE WILL BE AFFECTED.")
                           (log/info "Guardrails was enabled because"
                             (if cljs-compiler-config
                               "the CLJS Compiler config enabled it"
                               "the guardrails.enabled property is set to a (any) value.")))
                         (merge {}
                           (read-config-file)
                           cljs-compiler-config)))]
          ;#?(:clj (.println System/err config)) ; DEBUG
          config))]

  (defn get-env-config
    ([]
     (get-env-config true))
    ([cache?]
     (let [result (if (or (not cache?)
                        #?(:clj (= (System/getProperty "guardrails.cache") "false")))
                    (reload-config)
                    (let [now (identity #?(:clj (System/currentTimeMillis) :cljs (js/Date.now)))]
                      (if (< (- now (::timestamp @*config-cache))
                            2000)
                        (::value @*config-cache)
                        (::value (reset! *config-cache {::timestamp now
                                                        ::value     (reload-config)})))))]
       #?(:clj (when (and result cljs-env/*compiler*)
                 (let [production? (contains? #{:advanced :whitespace :simple} (get-in @cljs-env/*compiler* [:options :optimizations]))]
                   (when (and production? (not= "production" (System/getProperty "guardrails.enabled")))
                     (throw (ex-info (str "REFUSING TO COMPILE PRODUCTION BUILD WITH GUARDRAILS ENABLED!.  If you really want to take "
                                          "that performance hit then set the JVM properter guardrails.enabled to \"production\" on the CLJS compiler's JVM")
                                     {}))))))
       result))))

(defn get-base-config-fn
  "Base config is defaults + env config."
  ([]
   (get-base-config-fn true))
  ([cache?]
   (->> (get-env-config cache?)
     (merge default-config))))

(defmacro get-base-config-macro
  ([]
   (get-base-config-fn))
  ([cache?]
   (get-base-config-fn cache?)))

(defn merge-config [env & meta-maps]
  (let [config (->> (apply merge-with
                      (fn [a b]
                        (if (every? map? [a b])
                          (merge a b)
                          b))
                      (get-base-config-fn)
                      (util/get-ns-meta env)
                      meta-maps)
                 (into {}))]
    config))
