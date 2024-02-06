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
    [clojure.string :as str]
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
  "Get a string that represents the full stack trace"
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

#?(:clj
   (def ^:dynamic *stacktrace-filters* ["java.lang."
                                        "com.fulcrologic.guardrails"
                                        "clojure.test$"
                                        "clojure.lang."
                                        "clojure.core"
                                        "clojure.main"
                                        "orchestra."
                                        "kaocha.monkey_patch"]))

(defn- elide-element? [e]
  #?(:clj  (let [ele-name (.getClassName ^StackTraceElement e)]
             (or
               (str/includes? ele-name "guardrails_wrapper")
               (some (fn [filter] (str/starts-with? ele-name filter)) *stacktrace-filters*)))
     :cljs false))

#?(:clj
   (defn- stack-trace-element [^StackTraceElement e]
     (let [class    (.getClassName e)
           method   (.getMethodName e)
           filename (or (.getFileName e) "")
           line     (.getLineNumber e)]
       (let [match (re-matches #"^([A-Za-z0-9_.-]+)\$(\w+)__\d+$" (str class))]
         [(if (and match (= "invoke" method))
            (apply format "%s/%s" (rest match))
            (format "%s.%s" class method))
          filename line]))))

(defn stack-trace
  "Returns a vector of a Clojure-oriented stack trace of tr a Throwable/Exception. In CLJS this is just `(vector tr)`."
  ([tr]
   (stack-trace tr false))
  ([tr prune?]
   #?(:clj  (let [st     (.getStackTrace tr)
                  result []]
              (loop [[e & st] (next st)
                     n            0
                     final-result result]
                (cond
                  (and prune? (> n 4)) final-result
                  e (if (and prune? (elide-element? e))
                      (recur st n final-result)
                      (recur st (inc n) (conj final-result (stack-trace-element e))))
                  :else final-result)))
      :cljs [(try (.-stack tr) (catch :default _ tr))])))

(declare current-backtrace)
(declare backtrace-entry-function)
(declare backtrace-entry-args)

(defn report-info [message]
  (println message))

(def -last-failure-map (volatile! {}))
(defn last-failure
  "Returns the stack trace of the most recent GR failure for the fully-qualified function name (string or symbol)
   `fnsym`.  `prune?` (default true) indicates that it should remove frames that appear to be uninteresting noise."
  ([fnsym] (last-failure fnsym true))
  ([fnsym prune?]
   (some-> @-last-failure-map
     (get (symbol fnsym))
     (stack-trace prune?))))

(defn record-failure [str-or-sym e]
  (vswap! -last-failure-map assoc (symbol str-or-sym) e))

(defn backtrace-str []
  (str/join "\n"
    (for [{:keys [f args]} (current-backtrace)
          :let [call (apply list (into [f] args))]]
      (str "    " (try (pr-str call)
                       (catch #?(:clj Throwable :cljs :default) _))))))

(defn problem-description [message callsite-ex {stack-trace-option :guardrails/stack-trace
                                                :guardrails/keys   [fqnm trace?] :as options}]
  (cond-> (str message "\n")
    callsite-ex (cond->
                  trace? (str "  GR functions on stack. (" `last-failure " '" (or fqnm "fn-sym") ") for full stack:\n" (backtrace-str) "\n")
                  :and (str (case stack-trace-option
                              :none nil
                              :prune (str
                                       "\nPruned Stack Trace (see `gr.utils/last-failure-stacktrace` for full trace)\n\n"
                                       (str/join " called by " (map pr-str
                                                                 (stack-trace callsite-ex true)))
                                       "\n")
                              (str/join "\n"
                                (stack-trace callsite-ex false)))
                         "\n"))))

(defn report-problem
  ([message] (report-problem message nil {}))
  ([message callsite-ex {stack-trace-option :guardrails/stack-trace
                         :guardrails/keys   [fqnm trace?] :as options}]
   (println (problem-description message callsite-ex options))))

(defn report-exception [e message]
  (println (str message "\n" (ex-message e) "\n" (some-> e stacktrace))))

(def ^:dynamic *backtrace* nil)

(defn backtrace-entry
  ([] [])
  ([nspc nm args] [nspc nm args]))

(def empty-entry (backtrace-entry))

(defn new-backtrace
  ([] (new-backtrace 5))
  ([sz] (let [bt (object-array (inc sz))]
          (aset bt 0 0)
          (doseq [n (range 1 (inc sz))]
            (aset bt n empty-entry))
          bt)))

(defn backtrace-enter
  [nspc nm & args]
  (when *backtrace*
    (let [next-entry (aget *backtrace* 0)
          new-entry  (backtrace-entry nspc nm args)
          sz         (dec (count *backtrace*))]
      (aset *backtrace* (+ 1 next-entry) new-entry)
      (aset *backtrace* 0 (mod (inc next-entry) sz)))))

(defn backtrace-exit []
  (when *backtrace*
    (let [next-entry  (aget *backtrace* 0)
          sz          (dec (count *backtrace*))
          prior-entry (mod (dec next-entry) sz)]
      (aset *backtrace* (+ 1 prior-entry) empty-entry)
      (aset *backtrace* 0 prior-entry))))

(defn backtrace-entry-function
  "Returns the function called in the given backtrace entry, or nil if the entry is nil/empty"
  [backtrace-entry]
  (if (or (nil? backtrace-entry) (= backtrace-entry empty-entry))
    nil
    (symbol (name (get backtrace-entry 0 "")) (name (get backtrace-entry 1 "")))))

(defn backtrace-entry-args
  "Returns the arguments passed to the call in the backtrace entry, or nil if empty"
  [backtrace-entry]
  (if (or (nil? backtrace-entry) (= backtrace-entry empty-entry))
    nil
    (vec (get backtrace-entry 2))))

(defn current-backtrace
  "Returns a vector of maps for the current backtrace."
  []
  (when *backtrace*
    (let [start (aget *backtrace* 0)
          sz    (dec (count *backtrace*))]
      (vec
        (for [n (range 1 (inc sz))
              :let [pos   (inc (mod (- start n) sz))
                    entry (aget *backtrace* pos)]
              :when (not= entry empty-entry)]
          {:f    (backtrace-entry-function entry)
           :args (backtrace-entry-args entry)})))))
