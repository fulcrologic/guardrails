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

#?(:clj
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

(let [ansi-color-regex #"\033\[[0-9;]*m"]
  (defn strip-colors [s]
    (clojure.string/replace s ansi-color-regex "")))


;; ADR: Printing to *err* during the compilation process breaks with nREPL, so we don't
;;
;; gnl:
;; The reason we're no longer printing to *err* below is due to an obscure nREPL
;; issue where doing it in this manner during the process of loading namespaces
;; appears to break the latter when using a Cursive (1.13.1-2023.3) remote REPL
;; with nREPL (1.1.0). Whether this might affect other nREPL client/server
;; combinations is unknown. Since Guardrails can emit various messages during
;; this process, this can lead to very strange and difficult to debug failures
;; completely unrelated to any errors in the actual code. Using `(.println
;; System/err ...)` does not seem to have this effect, however it only prints to
;; the terminal where the nREPL server is running, rather than the nREPL client
;; (which has been a known nREPL issue in various environments).
;;
;; If someone with the necessary courage, pain tolerance and free time on their
;; hands should feel the need to delve into this at some point in the future,
;; here's a basic repro that should make things blow up:
;;
;; - Uncomment everything related to `*err*` below and correctly wrap the
;; relevant forms in `binding`
;; - Require `com.fulcrologic.guardrails.core` in the `user` namespace
;; - Define a `com.fulcrologic.guardrails.core/>defn` function in some namespace
;; in your application
;; - Launch an nREPL server with this deps.edn alias:
;; :clj {:extra-deps {nrepl/nrepl {:mvn/version "1.1.0"}}
;;       :main-opts  ["-m" "nrepl.cmdline"]}
;; - Create a Cursive remote nREPL run config and execute it
;; - Load the namespace where you used `>defn` and watch the process die quietly
;; in the REPL right after Guardrails tells you that it's been enabled
;; - Simply removing the `[*out* *err*]` bindings around `println` makes it work
;; again.
;;
;; If you only require `com.fulcrologic.guardrails.core` in the namespace where
;; you use `>defn`, you should be able to make it fail only on the first load
;; attempt (when the initial Guardrails messages are printed), and complete
;; successfully on subsequent attempts (when Guardrails is quiet).
;;
;; For now, given the above as well as the long and ongoing history of various
;; stderr-related nREPL client and server issues, we're simply no longer
;; printing anything non-fatal to stderr.

;#?(:cljs
;   (def ^:dynamic *err* *out*))

(defn report-info [message]
  #_(binding [*out* *err*])
  (println message))

(defn report-problem [message]
  #_(binding [*out* *err*])
  (println message))

(defn report-exception [e message]
  #_(binding [*out* *err*])
  (println (str message "\n" (ex-message e) "\n" (some-> e stacktrace))))
