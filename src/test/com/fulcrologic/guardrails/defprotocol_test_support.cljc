(ns com.fulcrologic.guardrails.defprotocol-test-support
  "Test helpers and the `target-checks` macro, which expands to LITERAL call sites `(m <target-expr> ...)` so that
   e.g. `(->Rec 1)` call sites are tag-inferred by the CLJS compiler."
  #?(:cljs (:require-macros com.fulcrologic.guardrails.defprotocol-test-support))
  (:require
   [clojure.string :as str]
   #?(:clj [clojure.test :as t] :cljs [cljs.test :as t])
   [fulcro-spec.core]))

(defn kind
  "Calls `f`: `:ok` on success, the guardrails failure point (`:args`/`:ret`) on a guardrails validation error,
   `[:error msg]` otherwise."
  [f]
  (try (f) :ok
       (catch #?(:clj Throwable :cljs :default) e
         (let [d (ex-data e)]
           (if (= :com.fulcrologic.guardrails/validation-error (:type d))
             (:com.fulcrologic.guardrails/failure-point d)
             [:error (ex-message e)])))))

(defn- classify [{:keys [message actual]}]
  (let [msg (str message " " actual)]
    (if (re-find #"(?i)(validation failed|doesn't match|don't match|wrong arity)" msg) :validation msg)))

(defn mock-run
  "Runs `f` with test failure reports captured. Returns `{:value v :fails [:validation | msg ...]}`."
  [f]
  (let [fails (atom [])
        v     (let [capture (fn [m] (when (#{:fail :error} (:type m)) (swap! fails conj (classify m))))
                    body    (fn [] (try (f) (catch #?(:clj Throwable :cljs :default) e [:thrown (ex-message e)])))]
                ;; CLJ binds the dynamic `report`, because `with-redefs` on the non-dynamic `do-report` has no effect
                ;; when fulcro-spec is compiled with direct linking.
                #?(:clj  (binding [t/report capture] (body))
                   :cljs (with-redefs [t/do-report capture] (body))))]
    {:value v :fails @fails}))

#?(:clj
   (defmacro target-checks
     "Assertions for one implementation `target` (an expression) of methods `m` (arity 2+3) and `z` (arity 1)."
     [m z label target]
     (let [cljs?   (boolean (:ns &env))
           testing (if cljs? 'cljs.test/testing 'clojure.test/testing)
           a       (gensym "a")
           b       (gensym "b")
           c       (gensym "c")
           =>      '=>]
       `(~testing ~label
          (fulcro-spec.core/assertions
            "ok arity-2" (kind (fn [] (~m ~target "a"))) ~=> :ok
            "ok arity-3" (kind (fn [] (~m ~target "a" 1))) ~=> :ok
            "ok arity-1" (kind (fn [] (~z ~target))) ~=> :ok
            "value is a string" (string? (~m ~target "a")) ~=> true
            "bad arg arity-2" (kind (fn [] (~m ~target 42))) ~=> :args
            "bad arg arity-3" (kind (fn [] (~m ~target "a" "not-int"))) ~=> :args
            "bad ret arity-2" (kind (fn [] (~m ~target "bad-ret"))) ~=> :ret
            "bad ret arity-3" (kind (fn [] (~m ~target "bad-ret" 1))) ~=> :ret
            "with-redefs arity-2" (with-redefs [~m (fn [& _#] "R")] (~m ~target "a")) ~=> "R"
            "with-redefs arity-3" (with-redefs [~m (fn [& _#] "R")] (~m ~target "a" 1)) ~=> "R"
            "with-redefs arity-1" (with-redefs [~z (fn [& _#] 99)] (~z ~target)) ~=> 99
            "when-mocking! ok" (mock-run (fn [] (fulcro-spec.core/when-mocking! ~(list m a b) ~=> "M" (~m ~target "a"))))
            ~=> {:value "M" :fails []}
            "when-mocking! ok arity-3" (mock-run (fn [] (fulcro-spec.core/when-mocking! ~(list m a b c) ~=> "M3" (~m ~target "a" 1))))
            ~=> {:value "M3" :fails []}
            "when-mocking! bad mock return" (:fails (mock-run (fn [] (fulcro-spec.core/when-mocking! ~(list m a b) ~=> 99 (~m ~target "a")))))
            ~=> [:validation]
            "when-mocking! bad args to mock" (:fails (mock-run (fn [] (fulcro-spec.core/when-mocking! ~(list m a b) ~=> "M" (~m ~target 42)))))
            ~=> [:validation]
            "when-mocking! arity-3 bad mock return" (:fails (mock-run (fn [] (fulcro-spec.core/when-mocking! ~(list m a b c) ~=> :kw (~m ~target "a" 1)))))
            ~=> [:validation]
            "when-mocking! arity-1 bad mock return" (:fails (mock-run (fn [] (fulcro-spec.core/when-mocking! ~(list z a) ~=> "not-int" (~z ~target)))))
            ~=> [:validation]
            "provided! ok" (mock-run (fn [] (let [v# (atom nil)]
                                              (fulcro-spec.core/provided! "mocked" ~(list m a b) ~=> "P" (reset! v# (~m ~target "a")))
                                              (deref v#))))
            ~=> {:value "P" :fails []}
            "provided! bad mock return" (:fails (mock-run (fn [] (fulcro-spec.core/provided! "mocked" ~(list m a b) ~=> 99 (~m ~target "a")))))
            ~=> [:validation]
            "restored after mocking" (kind (fn [] (~m ~target 42))) ~=> :args)))))
