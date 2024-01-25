;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;; Some additions by Tony Kay, Fulcrologic, LLC

(ns com.fulcrologic.guardrails.config
  "Manages the guardrails configuration. Most of this is automatic, and is controlled by files you place on the
   filesystem and possibly reference by system properties (which you can set on the JVM with the -D option).

   Controlling Guardrails Checks at Runtime

   Normally if guardrails is enabled, then the macros (e.g. `>defn`) emit code that will cause checking to happen
   on every call. This is fine when you are working on files you own, but as your project grows, and you possibly
   bring in libraries that also use Guardrails you may find that runtime checking adds significant (and unacceptable)
   overhead to your development environment.

   As a library author, you can include a file named `guardrails-export.edn` in the root of your main source code
   (making sure it gets copied into your distribution jar at the top-level). This file can be used to auto-exclude
   namespaces or functions from runtime checks, so that users of your library do not take an unnecessary performance hit.

   The idea is that all internal implementation can have GR turned off because you tested your library
   and are sure you're doing internal cross-calls correctly.

   So, your library can leave checking turned on at the core API interface level.

   Say you have these namespaces in your library:

   ```
   com.project.impl
   com.project.other
   com.project.core
   ```

   You might include the following `guardrails-export.edn` in your `src` folder:

   ```
   {:exclude #{com.project.impl com.project.other}}
   ```

   Now users of your library that also use GR will not take a performance hit on your internal implementation.

   An as end user you can *always* choose to turn checking on/off anywhere/everywhere. See:

   allow-checks! - Enable checking on something that was excluded
   exclude-checks! - Disable checking on something
   clear-exclusions! - Enable checking EVERYWHERE
   reset-exclusions! - Bring the exclusions back to load-time default (library export values)
   excluded? - Check if a function/ns is currently being excluded

   Disabling Exclusions Globally (for CI/Testing)

   There are times when you might want the exclusions to not apply. For example as a library author running
   local tests from the CLI (e.g. kaocha) you don't want your exclusions being included.

   You can globally disable exclusions by choosing an alternate config file (with JVM `-Dguardrails.config=filename`)
   and adding a flag:

   ```
   {:throw?              true
    :disable-exclusions? true}
   ```

   This will cause a runtime check on all instrumented functions everywhere, including other libraries.

   Your alternative is to make sure that something like `clear-exclusions!` runs before any of your tests.

   Improving Performance with Throttled Checking

   Guardrails normally runs checks on every call. For functions that are called in tight loops this is too much, and
   leads to serious slowdowns. Excluding the fn or namespace fixes this, but then you have to visibility for problems.
   An alternative is to ask Guardrails to limit the number of checks to a specific rate/s. This can be done globally,
   by namespace, or by function (anywhere you can put guardrails config. The option is :guardrails/mcps (max
   checks per second). The first call is always checked, and then the rate is enforced at ns accuracy at each additional
   call. Of course this is affected by various factors (timer jitter, etc.), and will give an approximate behavior.

   The overhead of the throttling code is designed to be very light. It isn't present at all unless enabled, and
   adds somewhere around 30ns of overhead per call. Much less than the roughly 10 microsecond overhead of an actual
   spec/return value check (almost 1000x faster).

   ```
   (>defn f
     {:guardrails/mcps 100}
     [n]
     [int? => int?]
     ...)
   ```
   "
  #?(:cljs (:require-macros com.fulcrologic.guardrails.config))
  (:require
    [com.fulcrologic.guardrails.utils :as utils]
    #?@(:clj  [[clojure.edn :as edn]
               [clojure.set :as set]]
        :cljs [[cljs.env :as cljs-env]])))

;; This isn't particularly pretty, but it's how we avoid
;; having ClojureScript as a required dependency on Clojure
#?(:bb  (require '[com.fulcrologic.guardrails.stubs.cljs-env :as cljs-env])
   :clj (try
          (ns-unalias (find-ns 'com.fulcrologic.guardrails.utils) 'cljs-env)
          (require '[cljs.env :as cljs-env])
          (catch Exception _
            (require '[com.fulcrologic.guardrails.stubs.cljs-env :as cljs-env]))))

(defn mode [config]
  (get config :mode :runtime))

(defn async? [config]
  (get config :async? false))

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
                   (edn/read-string
                     (slurp
                       (or (System/getProperty "guardrails.config")
                         "guardrails.edn")))
                   (catch Exception _ nil))
           :cljs nil))

      reload-config
      (fn []
        ;#?(:clj (.println System/err (get @cljs-env/*compiler* :options))) ; DEBUG
        (let [config (let [cljs-compiler-config
                           (when cljs-env/*compiler*
                             (get-in @cljs-env/*compiler* [:options :external-config :guardrails]))]
                       (when #?(:clj  (or
                                        cljs-compiler-config
                                        (System/getProperty "guardrails.enabled"))
                                :cljs false)
                         (let [{:keys [async? throw?] :as result} (merge {} (read-config-file) cljs-compiler-config)
                               result (if (and async? throw?)
                                        (dissoc result :async?)
                                        result)]
                           (when-not @warned?
                             (reset! warned? true)
                             (utils/report-info "GUARDRAILS IS ENABLED. RUNTIME PERFORMANCE WILL BE AFFECTED.")
                             (when (and async? throw?)
                               (utils/report-problem "INCOMPATIBLE MODES: :throw? and :async? cannot both be true. Disabling async."))
                             (utils/report-info (str "Mode: " (mode result) (when (= :runtime (mode result))
                                                                              (str "  Async? " (boolean (:async? result))
                                                                                "  Throw? " (boolean (:throw? result))))))
                             (utils/report-info (str "Guardrails was enabled because "
                                                  (if cljs-compiler-config
                                                    "the CLJS Compiler config enabled it"
                                                    "the guardrails.enabled property is set to a (any) value."))))
                           result)))]
          ;#?(:clj (.println System/err config)) ; DEBUG
          config))]

  (defn get-env-config
    ([]
     (get-env-config true))
    ([cache?]
     (let [result               (if (or (not cache?)
                                      #?(:clj (= "false" (System/getProperty "guardrails.cache"))))
                                  (reload-config)
                                  (let [now        (identity #?(:clj (System/currentTimeMillis) :cljs (js/Date.now)))
                                        since-last (- now (::timestamp @*config-cache))]
                                    (if (< since-last 2000)
                                      (::value @*config-cache)
                                      (::value (reset! *config-cache {::timestamp now
                                                                      ::value     (reload-config)})))))
           cljs-compiler-config (when cljs-env/*compiler*
                                  (get-in @cljs-env/*compiler* [:options :external-config :guardrails]))
           mode-config #?(:cljs nil
                          :clj  (when-let [mode (System/getProperty "guardrails.mode")]
                                  (let [?mode (read-string mode)]
                                    (if (#{:runtime :pro :all :copilot} ?mode)
                                      {:mode ?mode}
                                      (.println System/err (format "Unknown guardrails mode %s, defaulting to :runtime" mode))))))]
       #?(:clj (when (and result cljs-env/*compiler*)
                 (let [production? (contains? #{:advanced :whitespace :simple}
                                     (get-in @cljs-env/*compiler* [:options :optimizations]))]
                   (when (and production? (not= "production" (System/getProperty "guardrails.enabled")))
                     (throw (ex-info (str "REFUSING TO COMPILE PRODUCTION BUILD WITH GUARDRAILS ENABLED!.  If you really want to take "
                                       "that performance hit then set the JVM properter guardrails.enabled to \"production\" on the CLJS compiler's JVM")
                              {}))))))
       (merge result cljs-compiler-config mode-config)))))

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
                      (utils/get-ns-meta env)
                      meta-maps)
                 (into {}))]
    config))

#?(:clj
   (defmacro ?runtime
     "If guardrails is enabled in runtime mode, then this evaluates to `v`, otherwise evaluates to nil (or dflt)."
     ([v] (?runtime v nil))
     ([v dflt]
      (let [cfg  (get-env-config)
            mode (mode cfg)]
        (if (and cfg (#{:runtime :all} mode))
          v
          dflt)))))

#?(:clj (def ^String export-file "guardrails-export.edn"))

#?(:clj
   (defn- export-urls
     "Internal. Find all of the export files on the classpath."
     []
     (let [cl (.. Thread currentThread getContextClassLoader)]
       (enumeration-seq (.getResources cl export-file)))))

#?(:clj
   (defn- load-export-file
     "Internal. Combine a classpath export file with master-config"
     [master-config ^java.net.URL url]
     (with-open [rdr (clojure.lang.LineNumberingPushbackReader.
                       (java.io.InputStreamReader.
                         (.openStream url) "UTF-8"))]
       (let [read-opts  {:eof nil}
             new-export (read read-opts rdr)]
         (if (or
               (not (map? new-export))
               (not (set? (:exclude new-export)))
               (not (every? symbol? (:exclude new-export))))
           (println (str "Not a valid guardrails export. Excluded key must be a set of symbols: " url))
           (update master-config :exclude set/union (into #{} (map keyword) (:exclude new-export))))))))

#?(:clj
   (defn ^:no-doc -load-exports
     "Internal. Loads all of the guardrails-export.edn files on the classpath, and returns a result of merging all of them
      together."
     []
     (reduce load-export-file {} (export-urls))))

#?(:clj
   (defmacro defexclusions [sym]
     (let [{:keys [exclude]} (-load-exports)
           exclusion-map (into {} (map (fn [s] [(keyword s) true])) exclude)]
       `(def ~sym (volatile! '~exclusion-map)))))

(com.fulcrologic.guardrails.config/defexclusions default-exclusions)
(com.fulcrologic.guardrails.config/defexclusions current-exclusions)

(defn reset-exclusions!
  "Reset the exclusions back to what they were when the system first loaded (what library authors auto-excluded)"
  []
  (vreset! current-exclusions @default-exclusions))

(defn clear-exclusions!
  "Clear all exclusions, so that checks are done for everything.

   All namespaces, even those that library authors have auto-excluded, will
   be checked. This can significantly slow your development REPL. The approximate overhead for the average
   check is about 10-30 microseconds. That is very small but in loops and such can add up to significant
   overhead (many functions take nanoseconds to run...so this can easily make your program run 100x slower."
  []
  (vreset! current-exclusions {}))

(defn exclude-checks!
  "Exclude a namespace or defn from checking. Can be a keyword or symbol. Giving a namespace will turn off all
   checks in that namespace. Giving a fn name will only affect that function."
  [ns-or-fn]
  {:pre [(or (keyword? ns-or-fn) (symbol? ns-or-fn))]}
  (vswap! current-exclusions assoc (keyword ns-or-fn) true))

(defn allow-checks!
  "Allow a namespace or >defn for checking. Can be a keyword or symbol. Giving an entire namespace
   will clear exclusions on all keys that use that ns. Giving a fn name will enable checking on that
   function, BUT keep a ns exclusion (if present)."
  [ns-or-fn]
  {:pre [(or (keyword? ns-or-fn) (symbol? ns-or-fn))]}
  (let [k        (keyword ns-or-fn)
        entry-ns (fn [k] (if (qualified-keyword? k) (namespace k) (name k)))
        kns      (entry-ns k)]
    (if (qualified-keyword? k)
      (vswap! current-exclusions assoc k false)
      (vswap! current-exclusions (fn [m]
                                   (reduce-kv
                                     (fn [acc ex v]
                                       (if (= (entry-ns ex) kns)
                                         acc
                                         (assoc acc ex v)))
                                     {}
                                     m))))))

(defn ^:no-doc -excluded?
  "INTERNAL. Do not use."
  [fqkw nskw]
  ;; We use keywords because they are faster to look up (they are interned), and precalc them in the macro at compile time
  (let [ex   @current-exclusions
        kval (get ex fqkw)]
    (or
      (true? kval)
      (and (not (false? kval)) (true? (get ex nskw))))))

(defn excluded?
  "Returns true if the fully-qualified-name matches something (namespace or fn) that is currently excluded from checks."
  [fully-qualified-name]
  (let [k       (keyword fully-qualified-name)
        q?      (qualified-keyword? k)
        nspc    (if q? (namespace k) (name k))
        fn-name (if q? (name k) nil)
        coord   [(when q? (keyword nspc (name fn-name))) (keyword nspc)]]
    (apply -excluded? coord)))
