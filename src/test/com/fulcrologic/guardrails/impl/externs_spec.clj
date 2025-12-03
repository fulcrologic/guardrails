(ns com.fulcrologic.guardrails.impl.externs-spec
  (:require
    [com.fulcrologic.guardrails.impl.externs :as gr.externs]
    [com.fulcrologic.guardrails.registry :as gr.reg]
    [fulcro-spec.core :refer [assertions behavior specification when-mocking]]))

(specification "extern-symbols"
  (when-mocking
    (gr.externs/cljc-resolve _ s) => {::MOCK ::VALUE}
    (assertions
      (gr.externs/extern-symbols {}
        '[(foo (bar) 0) (qux 1 [2 extern/three])])
      => (zipmap [''foo ''bar ''qux ''extern/three]
           (repeat {::MOCK ::VALUE})))))

;; ============================================================================
;; Call Graph Analysis Tests
;; ============================================================================

(defn setup-test-registries!
  "Sets up test data in the externs-registry and function-registry for testing call graph functions.
   Creates a simple call graph:
     myapp.core/main -> myapp.core/helper, myapp.util/format
     myapp.core/helper -> myapp.util/format
     myapp.util/format -> (no calls)
     other.ns/foo -> myapp.core/main"
  []
  (reset! gr.externs/externs-registry
    {"myapp.core" {'main   {''myapp.core/helper {::gr.reg/extern-name '(quote myapp.core/helper)
                                                 ::gr.reg/macro?      false}
                            ''myapp.util/format {::gr.reg/extern-name '(quote myapp.util/format)
                                                 ::gr.reg/macro?      false}
                            ''clojure.core/inc  {::gr.reg/extern-name '(quote clojure.core/inc)
                                                 ::gr.reg/macro?      false}}
                   'helper {''myapp.util/format {::gr.reg/extern-name '(quote myapp.util/format)
                                                 ::gr.reg/macro?      false}}}
     "myapp.util" {'format {''clojure.core/str {::gr.reg/extern-name '(quote clojure.core/str)
                                                ::gr.reg/macro?      false}}}
     "other.ns"   {'foo {''myapp.core/main {::gr.reg/extern-name '(quote myapp.core/main)
                                            ::gr.reg/macro?      false}}}})
  (reset! gr.externs/function-registry
    {"myapp.core" {'main   {::gr.reg/fn-name 'main}
                   'helper {::gr.reg/fn-name 'helper}}
     "myapp.util" {'format {::gr.reg/fn-name 'format}}
     "other.ns"   {'foo {::gr.reg/fn-name 'foo}}}))

(defn cleanup-test-registries! []
  (reset! gr.externs/externs-registry {})
  (reset! gr.externs/function-registry {}))

(specification "get-externs"
  (behavior "returns externs map for a registered function"
    (try
      (setup-test-registries!)
      (assertions
        (gr.externs/get-externs 'myapp.core/main)
        => {''myapp.core/helper {::gr.reg/extern-name '(quote myapp.core/helper)
                                 ::gr.reg/macro?      false}
            ''myapp.util/format {::gr.reg/extern-name '(quote myapp.util/format)
                                 ::gr.reg/macro?      false}
            ''clojure.core/inc  {::gr.reg/extern-name '(quote clojure.core/inc)
                                 ::gr.reg/macro?      false}})
      (finally
        (cleanup-test-registries!))))

  (behavior "returns nil for unregistered function"
    (try
      (setup-test-registries!)
      (assertions
        (gr.externs/get-externs 'nonexistent/fn) => nil)
      (finally
        (cleanup-test-registries!)))))

(specification "in-scope?"
  (behavior "returns true when symbol namespace matches a prefix"
    (assertions
      (gr.externs/in-scope? 'myapp.core/foo #{"myapp"}) => true
      (gr.externs/in-scope? 'myapp.util/bar #{"myapp"}) => true
      (gr.externs/in-scope? 'myapp.core/foo #{"myapp.core"}) => true))

  (behavior "returns falsy when symbol namespace doesn't match any prefix"
    (assertions
      (gr.externs/in-scope? 'other.ns/foo #{"myapp"}) => nil
      (gr.externs/in-scope? 'clojure.core/inc #{"myapp"}) => nil))

  (behavior "returns nil for symbols without namespace"
    (assertions
      (gr.externs/in-scope? 'foo #{"myapp"}) => nil)))

(specification "direct-calls"
  (behavior "returns set of in-scope functions directly called"
    (try
      (setup-test-registries!)
      (assertions
        (gr.externs/direct-calls 'myapp.core/main #{"myapp"})
        => #{'myapp.core/helper 'myapp.util/format}

        (gr.externs/direct-calls 'myapp.core/helper #{"myapp"})
        => #{'myapp.util/format})
      (finally
        (cleanup-test-registries!))))

  (behavior "excludes functions outside scope"
    (try
      (setup-test-registries!)
      (assertions
        ;; clojure.core/inc is called but not in myapp scope
        (contains? (gr.externs/direct-calls 'myapp.core/main #{"myapp"}) 'clojure.core/inc)
        => false)
      (finally
        (cleanup-test-registries!))))

  (behavior "returns nil for unregistered function"
    (try
      (setup-test-registries!)
      (assertions
        (gr.externs/direct-calls 'nonexistent/fn #{"myapp"}) => nil)
      (finally
        (cleanup-test-registries!))))

  (behavior "returns empty set when function has no in-scope calls"
    (try
      (setup-test-registries!)
      (assertions
        ;; myapp.util/format only calls clojure.core/str which is out of scope
        (gr.externs/direct-calls 'myapp.util/format #{"myapp"}) => #{})
      (finally
        (cleanup-test-registries!)))))

(specification "all-in-scope-functions"
  (behavior "returns all functions in namespaces matching scope prefixes"
    (try
      (setup-test-registries!)
      (assertions
        (gr.externs/all-in-scope-functions #{"myapp"})
        => #{'myapp.core/main 'myapp.core/helper 'myapp.util/format}

        (gr.externs/all-in-scope-functions #{"myapp.core"})
        => #{'myapp.core/main 'myapp.core/helper}

        (gr.externs/all-in-scope-functions #{"other"})
        => #{'other.ns/foo})
      (finally
        (cleanup-test-registries!))))

  (behavior "returns empty set when no functions match scope"
    (try
      (setup-test-registries!)
      (assertions
        (gr.externs/all-in-scope-functions #{"nonexistent"}) => #{})
      (finally
        (cleanup-test-registries!)))))

(specification "transitive-calls"
  (behavior "returns all transitively called functions including the starting function"
    (try
      (setup-test-registries!)
      (assertions
        ;; main calls helper and format, helper calls format
        (gr.externs/transitive-calls 'myapp.core/main #{"myapp"})
        => #{'myapp.core/main 'myapp.core/helper 'myapp.util/format}

        ;; helper only calls format
        (gr.externs/transitive-calls 'myapp.core/helper #{"myapp"})
        => #{'myapp.core/helper 'myapp.util/format}

        ;; format has no in-scope calls
        (gr.externs/transitive-calls 'myapp.util/format #{"myapp"})
        => #{'myapp.util/format})
      (finally
        (cleanup-test-registries!))))

  (behavior "handles functions not in registry"
    (try
      (setup-test-registries!)
      (assertions
        ;; Starting from unregistered function should just return that function
        (gr.externs/transitive-calls 'nonexistent/fn #{"myapp"})
        => #{'nonexistent/fn})
      (finally
        (cleanup-test-registries!)))))

(specification "call-graph"
  (behavior "returns map of all functions to their direct calls within scope"
    (try
      (setup-test-registries!)
      (assertions
        (gr.externs/call-graph #{"myapp"})
        => {'myapp.core/main   #{'myapp.core/helper 'myapp.util/format}
            'myapp.core/helper #{'myapp.util/format}
            'myapp.util/format #{}})
      (finally
        (cleanup-test-registries!))))

  (behavior "returns empty map when no functions in scope"
    (try
      (setup-test-registries!)
      (assertions
        (gr.externs/call-graph #{"nonexistent"}) => {})
      (finally
        (cleanup-test-registries!)))))
