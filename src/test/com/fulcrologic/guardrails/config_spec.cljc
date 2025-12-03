(ns com.fulcrologic.guardrails.config-spec
  (:require
    [com.fulcrologic.guardrails.config :as c]
    [com.fulcrologic.guardrails.malli.core :refer [>defn =>]]
    [fulcro-spec.core :refer [assertions specification component =throws=>]])
  #?(:clj (:import [java.io ByteArrayOutputStream PrintStream])))

(>defn f
  [x]
  [int? => int?]
  x)

(>defn g
  [x]
  [int? => int?]
  x)

(specification "Exclusions system"
  (let [original @c/current-exclusions]
    (c/clear-exclusions!)

    (c/exclude-checks! 'a.b)
    (c/allow-checks! 'a.b/d)

    (assertions
      "Allows ns and kw selection"
      (c/-excluded? :a.b/d :a.b) => false
      (c/excluded? :a.b/d) => false
      (c/-excluded? :a.b/c :a.b) => true
      (c/excluded? :a.b/c) => true)

    (c/allow-checks! 'a.b)

    (assertions
      "Allows ns and kw selection"
      (c/-excluded? :a.b/d :a.b) => false
      (c/-excluded? :a.b/c :a.b) => false
      (c/excluded? :a.b/d) => false
      (c/excluded? :a.b/c) => false)

    (c/reset-exclusions!)
    (assertions
      "Can be reset back to the originally loaded exclusions"
      @c/current-exclusions => original))

  (component "Controls defn checking"
    (c/clear-exclusions!)
    (assertions
      "Enforces when not excluded"
      (f 1.2) =throws=> #"should be an int")
    (c/exclude-checks! `f)
    (assertions
      "Ignores when excluded"
      (f 1.2) => 1.2
      "Applies at the function level"
      (g 1.2) =throws=> #"should be an int")))

(specification "mode function"
  (assertions
    "Returns :runtime as default when config is nil"
    (c/mode nil) => :runtime
    "Returns :runtime as default when config has no :mode key"
    (c/mode {}) => :runtime
    "Returns the mode when present in config"
    (c/mode {:mode :pro}) => :pro
    (c/mode {:mode :all}) => :all
    (c/mode {:mode :runtime}) => :runtime))

#?(:clj
   (specification "Mode property parsing"
     (let [original-mode  (System/getProperty "guardrails.mode")
           capture-stderr (fn [f]
                            (let [baos     (ByteArrayOutputStream.)
                                  ps       (PrintStream. baos)
                                  original System/err]
                              (System/setErr ps)
                              (try
                                (f)
                                (finally
                                  (System/setErr original)))
                              (.toString baos)))]
       (try
         (component "Valid modes with keyword syntax"
           (System/setProperty "guardrails.mode" ":pro")
           (let [cfg (c/get-env-config false)]
             (assertions
               (c/mode cfg) => :pro))

           (System/setProperty "guardrails.mode" ":all")
           (let [cfg (c/get-env-config false)]
             (assertions
               (c/mode cfg) => :all))

           (System/setProperty "guardrails.mode" ":runtime")
           (let [cfg (c/get-env-config false)]
             (assertions
               (c/mode cfg) => :runtime)))

         (component "Valid modes with symbol syntax (no colon)"
           (System/setProperty "guardrails.mode" "pro")
           (let [cfg (c/get-env-config false)]
             (assertions
               (c/mode cfg) => :pro))

           (System/setProperty "guardrails.mode" "all")
           (let [cfg (c/get-env-config false)]
             (assertions
               (c/mode cfg) => :all)))

         (component "Invalid mode defaults to :runtime with warning"
           (System/setProperty "guardrails.mode" ":invalid")
           (let [stderr (capture-stderr #(c/get-env-config false))
                 cfg    (c/get-env-config false)]
             (assertions
               "Defaults to :runtime"
               (c/mode cfg) => :runtime
               "Prints warning to stderr"
               (re-find #"Unknown guardrails mode" stderr) =fn=> some?)))

         (component "No mode specified defaults to :runtime without warning"
           (System/clearProperty "guardrails.mode")
           (let [stderr (capture-stderr #(c/get-env-config false))
                 cfg    (c/get-env-config false)]
             (assertions
               "Defaults to :runtime"
               (c/mode cfg) => :runtime
               "Does not print warning to stderr"
               (re-find #"Unknown guardrails mode" stderr) => nil)))

         (finally
           (if original-mode
             (System/setProperty "guardrails.mode" original-mode)
             (System/clearProperty "guardrails.mode")))))))
