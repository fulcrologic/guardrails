(ns com.fulcrologic.guardrails.defprotocol-spec
  (:require
   [clojure.spec.alpha :as s]
   [com.fulcrologic.guardrails.config :as config]
   [com.fulcrologic.guardrails.core :as gr]
   [com.fulcrologic.guardrails.defprotocol-fixtures :as fx :refer [->DT ->IT ->Rec a-meta a-reify bar baz mbar mbaz mp nested
                                                                    #?@(:clj [->MapT] :cljs [a-specify])]]
   [com.fulcrologic.guardrails.defprotocol-test-support :refer [kind mock-run target-checks]]
   [com.fulcrologic.guardrails.impl.externs :as gr.externs]
   [com.fulcrologic.guardrails.malli.registry :as gr.malli.reg]
   [com.fulcrologic.guardrails.noop :as noop]
   [fulcro-spec.core :refer [=throws=> assertions provided specification when-mocking!]]
   [malli.core :as m])
  #?(:clj (:import (com.fulcrologic.guardrails.defprotocol_fixtures Rec))))

(specification "Spec flavor >defprotocol: dispatch, checks, and mocking per implementation"
  (target-checks bar baz "extend-type deftype" (->DT))
  (target-checks bar baz "inline defrecord (tag-inferred site)" (->Rec 1))
  (target-checks bar baz "inline deftype" (->IT))
  (target-checks bar baz "reify" (a-reify))
  (target-checks bar baz "extend-protocol string" "s")
  (target-checks bar baz "extend-protocol number" 7)
  (target-checks bar baz "extend-protocol nil" nil)
  (target-checks bar baz "extend-protocol Object/default" :kw)
  (target-checks bar baz "extend-via-metadata" (a-meta))
  #?(:clj (target-checks bar baz "extend with keyword map" (->MapT)))
  #?(:cljs (target-checks bar baz "specify!" (a-specify))))

(specification "Malli flavor >defprotocol: dispatch, checks, and mocking per implementation"
  (target-checks mbar mbaz "extend-type deftype" (->DT))
  (target-checks mbar mbaz "inline defrecord (tag-inferred site)" (->Rec 1))
  (target-checks mbar mbaz "inline deftype" (->IT))
  (target-checks mbar mbaz "reify" (a-reify))
  (target-checks mbar mbaz "extend-protocol string" "s")
  (target-checks mbar mbaz "extend-protocol number" 7)
  (target-checks mbar mbaz "extend-protocol nil" nil)
  (target-checks mbar mbaz "extend-protocol Object/default" :kw)
  (target-checks mbar mbaz "extend-via-metadata" (a-meta))
  #?(:clj (target-checks mbar mbaz "extend with keyword map" (->MapT)))
  #?(:cljs (target-checks mbar mbaz "specify!" (a-specify))))

(specification "Type-hinted and inline call sites of >defprotocol methods"
  (let [^Rec r (->Rec 1)]
    (assertions
      "check arguments through a ^Rec hinted call (spec)"
      (kind (fn [] (bar r 42))) => :args
      "check arguments through a ^Rec hinted call (malli)"
      (kind (fn [] (mbar r 42))) => :args
      "honor with-redefs at a ^Rec hinted call"
      (with-redefs [bar (fn [& _] "R")] (bar r "a")) => "R"
      "validate when-mocking! mocks at a ^Rec hinted call"
      (:fails (mock-run (fn [] (when-mocking! (bar x y) => 99 (bar r "a"))))) => [:validation]))
  #?(:cljs
     (let [^not-native r (->Rec 1)]
       (assertions
         "check arguments through a ^not-native hinted call"
         (kind (fn [] (bar r 42))) => :args
         "honor with-redefs at a ^not-native hinted call"
         (with-redefs [bar (fn [& _] "R")] (bar r "a")) => "R")))
  (assertions
    "run the real implementation for a self-call inside an inline impl"
    (baz (->Rec 1)) => 8
    "make a self-call inside an inline impl mockable (spec)"
    (with-redefs [bar (fn [& _] "XY")] (baz (->Rec 1))) => 2
    "make a self-call inside an inline impl mockable (malli)"
    (with-redefs [mbar (fn [& _] "XYZ")] (mbaz (->Rec 1))) => 3
    "check a self-call inside an inline impl"
    (with-redefs [fx/out (fn [_ _] 42)] (kind (fn [] (baz (->Rec 1))))) => :ret
    "check calls through higher-order use (map)"
    (kind (fn [] (doall (map bar [(->Rec 1)] [42])))) => :args
    "check calls through apply"
    (kind (fn [] (apply mbar (->Rec 1) [42]))) => :args))

(specification ">defprotocol protocol plumbing"
  (assertions
    "keeps satisfies? working"
    (satisfies? fx/Foo (->Rec 1)) => true
    "defines ::Foo as a spec for implementations"
    (s/valid? ::fx/Foo (->Rec 1)) => true
    (s/valid? ::fx/Foo (a-meta)) => true
    "defines ::MFoo as a Malli schema for implementations"
    (m/validate ::fx/MFoo (->Rec 1) {:registry gr.malli.reg/registry}) => true
    (m/validate ::fx/MFoo (a-meta) {:registry gr.malli.reg/registry}) => true
    "registers an s/fdef for spec-flavor methods"
    (some? (s/get-spec `bar)) => true
    "puts the Malli function schema on malli-flavor method vars"
    (:malli/schema (meta (var mbar))) => [:function
                                          [:=> [:catn [:this ::fx/MFoo] [:arg1 :string]] :string]
                                          [:=> [:catn [:this ::fx/MFoo] [:arg1 :string] [:arg2 :int]] :string]]
    "keeps method docstrings"
    (:doc (meta (var bar))) => "bar doc"
    "synthesizes [this argN] arglists"
    (:arglists (meta (var bar))) => '([this arg1] [this arg1 arg2])))

(specification ">defprotocol used in a non-top-level position"
  (assertions
    "dispatches to the implementation"
    (nested "x" "a") => "x:a"
    "checks the methods"
    (kind (fn [] (nested "x" 42))) => :args
    "registers the method's s/fdef"
    (some? (s/get-spec `nested)) => true
    "makes the methods mockable"
    (with-redefs [nested (fn [& _] "R")] (nested "x" "a")) => "R"
    (:fails (mock-run (fn [] (when-mocking! (nested x y) => 99 (nested "x" "a"))))) => [:validation]))

(specification ">defprotocol method var metadata"
  (assertions
    "keeps non-config metadata of the protocol name off the methods"
    (:private (meta (var mp))) => nil
    (:no-doc (meta (var mp))) => nil
    "applies guardrails config on the protocol name to the methods (here :throw? false)"
    (kind (fn [] (mp "not-an-int"))) => :ok
    "is the same on CLJ and CLJS (docstring, arglists, guardrails marker)"
    (select-keys (meta (var bar)) [:doc :arglists ::gr/guardrails])
    => {:doc "bar doc" :arglists '([this arg1] [this arg1 arg2]) ::gr/guardrails true}))

#?(:clj
   (defn- eval-in
     "Evaluates `form` with `*ns*` bound to the namespace `nsym`."
     [nsym form]
     (binding [*ns* (the-ns nsym)] (eval form))))

#?(:clj
   (specification ">defprotocol in :pro mode"
     (let [nsym 'com.fulcrologic.guardrails.defprotocol-pro-sample]
       (try
         (binding [*ns* (create-ns nsym)]
           (refer-clojure)
           (require '[com.fulcrologic.guardrails.core :as gr]))
         (provided "Guardrails is in :pro mode while the protocol is compiled"
           (config/get-env-config & _) => {:mode :pro}
           (eval-in nsym '(do
                            (gr/>defprotocol PP
                              (pm [:com.fulcrologic.guardrails.defprotocol-pro-sample/PP string? => string?]))
                            (deftype IT [] PP (pm [_ s] (str "IT:" s)))
                            (defn call-pm [x s] (pm x s)))))
         (assertions
           "emits no checks"
           ((ns-resolve nsym 'call-pm) ((ns-resolve nsym '->IT)) 42) => "IT:42"
           "keeps calls against inline implementations mockable"
           (eval-in nsym '(with-redefs [pm (fn [& _] "R")] (call-pm (->IT) "a"))) => "R"
           "registers the methods with the function registry"
           (-> (gr.externs/function-info (symbol (str nsym) "pm")) :com.fulcrologic.guardrails.registry/arities keys set)
           => #{2})
         (finally
           (remove-ns nsym))))))

#?(:clj
   (specification ">defprotocol used in a non-top-level position (CLJ, evaluated)"
     (let [nsym 'com.fulcrologic.guardrails.defprotocol-nested-sample]
       (try
         (binding [*ns* (create-ns nsym)]
           (refer-clojure)
           (require '[com.fulcrologic.guardrails.core :as gr]))
         (eval-in nsym '(when true
                          (gr/>defprotocol GrNT (gnt [:com.fulcrologic.guardrails.defprotocol-nested-sample/GrNT => int?]))
                          (extend-protocol GrNT String (gnt [this] (count this)))))
         (assertions
           "defines checked methods"
           ((ns-resolve nsym 'gnt) "abc") => 3
           (kind (fn [] ((ns-resolve nsym 'gnt) 42))) => :args
           "registers the s/fdef under the qualified method name"
           (some? (s/get-spec (symbol (str nsym) "gnt"))) => true
           "registers the method with the function registry"
           (some? (gr.externs/function-info (symbol (str nsym) "gnt"))) => true)
         (finally
           (remove-ns nsym))))))

#?(:clj
   (specification ">defprotocol function registration (:all mode)"
     ;; Evaluated at test time in a scratch namespace, because other specs reset the global function registry.
     (let [nsym 'com.fulcrologic.guardrails.defprotocol-registration-sample]
       (try
         (binding [*ns* (create-ns nsym)]
           (refer-clojure)
           (require '[com.fulcrologic.guardrails.core :as gr])
           (eval '(gr/>defprotocol P (pm [:com.fulcrologic.guardrails.defprotocol-registration-sample/P string? => string?]
                                       [:com.fulcrologic.guardrails.defprotocol-registration-sample/P string? int? => string?])))
           (eval '(extend-protocol P String (pm ([this s] (str this s)) ([this s n] (str this s n))))))
         (let [info (gr.externs/function-info (symbol (str nsym) "pm"))]
           (assertions
             "registers each method with its arities"
             (-> info :com.fulcrologic.guardrails.registry/arities keys set) => #{2 3}
             "registers the method var as the fn reference, so extensions made after registration are seen"
             ((:com.fulcrologic.guardrails.registry/fn-ref info) "a" "b") => "ab"))
         (finally
           (remove-ns nsym))))))

#?(:clj
   (specification ">defprotocol honors guardrails exclusions"
     (let [original @config/current-exclusions]
       (try
         (config/exclude-checks! `bar)
         (assertions
           "skips checks of an excluded method (an invalid return passes through)"
           (bar (->Rec 1) "bad-ret") => 42
           "keeps checking methods that are not excluded"
           (kind (fn [] (mbar (->Rec 1) "bad-ret"))) => :ret)
         (config/allow-checks! `bar)
         (assertions
           "checks the method again once it is allowed"
           (kind (fn [] (bar (->Rec 1) "bad-ret"))) => :ret)
         (finally
           (vreset! config/current-exclusions original))))))

#?(:clj
   (def ^:private sample-body '("Protocol doc"
                                (bar [::Foo string? => string?] [::Foo string? int? => string?] "bar doc"))))

#?(:clj
   (defn- heads
     "Returns the head symbol of each form of the `(do ...)` `expansion`."
     [expansion]
     (mapv #(if (seq? %) (first %) %) (rest expansion))))

#?(:clj
   (defn- mentions?
     "Returns true if `sym` occurs anywhere in `form`."
     [sym form]
     (boolean (some #{sym} (tree-seq coll? seq form)))))

#?(:clj
   (specification ">defprotocol* emission per guardrails mode"
     (let [opts   {:guardrails/def-sym `gr/>def}
           expand (fn [] (gr/>defprotocol* {} nil 'Foo sample-body opts))]
       (provided "There is no config (guardrails disabled)"
         (config/get-env-config & _) => nil
         (let [out (expand)]
           (assertions
             "emits a plain defprotocol with synthesized arglists"
             (second out) => '(clojure.core/defprotocol Foo "Protocol doc" (bar [this arg1] [this arg1 arg2] "bar doc"))
             "emits only the defprotocol, the ::Foo def, and the protocol name"
             (heads out) => `[defprotocol gr/>def quote]
             "defines the ns-qualified protocol keyword"
             (second (nth out 2)) => (keyword (str (ns-name *ns*)) "Foo"))))
       (provided "Guardrails is in :pro mode"
         (config/get-env-config & _) => {:mode :pro}
         (let [out (expand)]
           (assertions
             "emits a defprotocol with unprotected method vars, plus function registration"
             (heads out) => `[gr/-restore-protocol-meta! defprotocol gr/>def gr/-unprotect-methods!
                              gr.externs/record-defn! quote]
             "emits no checking code"
             (mentions? `gr/-install-checks! out) => false)))
       (provided "Guardrails is in :runtime mode"
         (config/get-env-config & _) => {:mode :runtime}
         (let [out (expand)]
           (assertions
             "installs checks"
             (mentions? `gr/-install-checks! out) => true
             "skips function registration"
             (mentions? `gr.externs/record-defn! out) => false)))
       (provided "Guardrails is in :all mode"
         (config/get-env-config & _) => {:mode :all}
         (let [out (expand)]
           (assertions
             "installs checks and registers the methods"
             (mentions? `gr/-install-checks! out) => true
             (mentions? `gr.externs/record-defn! out) => true)))
       (provided "Guardrails has a max checks per second on the protocol"
         (config/get-env-config & _) => {:mode :runtime}
         (assertions
           "emits throttling for the methods"
           (mentions? '__gr_vncalls (gr/>defprotocol* {} nil (with-meta 'Foo {:guardrails/mcps 10}) sample-body opts)) => true
           (mentions? '__gr_vncalls (expand)) => false)))))

#?(:clj
   (specification "noop >defprotocol"
     (assertions
       "expands to a plain defprotocol"
       (macroexpand-1 `(noop/>defprotocol ~'Foo ~@sample-body))
       => '(clojure.core/defprotocol Foo "Protocol doc" (bar [this arg1] [this arg1 arg2] "bar doc")))))

#?(:clj
   (specification ">defprotocol macro-time errors"
     (let [expand (fn [& body] (gr/>defprotocol* {} nil 'Foo body {:guardrails/def-sym `gr/>def}))]
       (assertions
         "rejects variadic gspecs"
         (expand '(bar [::Foo string? & string? => string?])) =throws=> #"Protocol methods cannot be variadic"
         "rejects more than one gspec for the same arity"
         (expand '(bar [::Foo string? => string?] [::Foo int? => string?])) =throws=> #"more than one gspec for arity 2"
         "rejects gspecs without a return spec"
         (expand '(bar [::Foo string?])) =throws=> #"without a return spec"
         "rejects gspecs without a `this` spec"
         (expand '(bar [=> string?])) =throws=> #"no argument specs"
         "rejects methods without a gspec"
         (expand '(bar "doc")) =throws=> #"has no gspec"
         "rejects arglists in place of gspecs"
         (expand '(bar (this x) [::Foo => int?])) =throws=> #"may only contain gspec vectors"
         "rejects duplicate methods"
         (expand '(bar [::Foo => int?]) '(bar [::Foo int? => int?])) =throws=> #"bar is declared more than once"
         "rejects an option without a value"
         (expand :extend-via-metadata) =throws=> #"missing a value"
         "rejects malformed gspecs"
         (expand '(bar [::Foo 42 => int?])) =throws=> #"invalid gspec for method bar"))))
