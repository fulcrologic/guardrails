;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^:no-doc com.fulcrologic.guardrails.utils
  #?(:cljs (:require-macros com.fulcrologic.guardrails.utils))
  (:require
    #?(:clj [clojure.stacktrace :as st])
    [clojure.walk :as walk]))

(defn cljs-env? [env] (boolean (:ns env)))

(defn get-ns-meta [env]
  (if (cljs-env? env)
    (or (meta *ns*) (some-> env :ns :meta))
    (meta *ns*)))

(defn get-ns-name [env]
  (if (cljs-env? env)
    (or (.-name *ns*) (some-> env :ns :name))
    (.-name *ns*)))

(defn clj->cljs
  ([form]
   (clj->cljs form true))
  ([form strip-core-ns]
   (let [ns-replacements   (cond-> {"clojure.core"            "cljs.core"
                                    "clojure.test"            "cljs.test"
                                    "clojure.spec.alpha"      "cljs.spec.alpha"
                                    "clojure.spec.test.alpha" "cljs.spec.test.alpha"
                                    "orchestra.spec.test"     "orchestra-cljs.spec.test"
                                    "clojure.spec.gen.alpha"  "cljs.spec.gen.alpha"}
                             strip-core-ns (merge {"clojure.core" nil
                                                   "cljs.core"    nil}))
         replace-namespace #(if-not (qualified-symbol? %)
                              %
                              (let [nspace (namespace %)]
                                (if (contains? ns-replacements nspace)
                                  (symbol (get ns-replacements nspace) (name %))
                                  %)))]
     (walk/postwalk replace-namespace form))))


(defn get-file-position
  [env]
  (if (cljs-env? env)
    (let [{:keys [line column]} env]
      (str line ":" column))
    ;; TODO implement for clojure
    nil))


(defn get-call-context
  ([env]
   (get-call-context env nil))
  ([env label]
   (str (when label (str label " – "))
     (get-ns-name env)
     ":"
     (get-file-position env))))


(defn gen-exception [env msg]
  `(throw (~(if (cljs-env? env) 'js/Error. 'Exception.) ~msg)))


(defn devtools-config-override
  []
  `(let [current-config# (~'devtools.prefs/get-prefs)
         overrides#      {:max-print-level                                    4
                          :min-expandable-sequable-count-for-well-known-types 2}
         left-adjust#    (str "margin-left: -17px;")]
     (merge current-config#
       (into overrides# (for [k# [:header-style]
                              :let [v# (get current-config# k#)]]
                          [k# (str v# left-adjust#)])))))

(defn map-vals [f m] (if (nil? m) {} (reduce-kv (fn [m k v] (assoc m k (f v))) m m)))
(defn map-keys [f m] (if (nil? m) {} (reduce-kv (fn [m k v] (assoc m (f k) v)) {} m)))
(let [p! persistent!, t transient]                          ; Note `mapv`-like nil->{} semantics
  (defn filter-vals [pred m] (if (nil? m) {} (p! (reduce-kv (fn [m k v] (if (pred v) m (dissoc! m k))) (t m) m)))))

#?(:bb
   (defn atom? [x] (satisfies? clojure.lang.IAtom x))
   :clj
   (defn atom? [x] (instance? clojure.lang.Atom x))
   :cljs
   (defn ^boolean atom? [x] (instance? Atom x)))

#?(:clj
   (defn compiling-cljs?
     "Return truthy iff currently generating Cljs code."
     []
     (when-let [n (find-ns 'cljs.analyzer)]
       (when-let [v (ns-resolve n '*cljs-file*)]
         @v))))

(defn stacktrace
  ([err] (stacktrace err nil))
  ([err opts]
   #?(:cljs (str err)
      :clj  (with-out-str (st/print-stack-trace err)))))

(defn report-problem [message]
  #?(:clj
     (.println System/err message)
     :cljs
     (js/console.error message)))

(defn report-exception [e message]
  #?(:clj
     (.println System/err (str message \n (.getMessage ^Exception e) "\n" (stacktrace e)))
     :cljs
     (js/console.error message e)))
