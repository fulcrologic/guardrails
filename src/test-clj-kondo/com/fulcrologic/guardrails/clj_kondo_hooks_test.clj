(ns com.fulcrologic.guardrails.clj-kondo-hooks-test
  (:require
    [clj-kondo.core :as clj-kondo]
    [clojure.test :refer [deftest is]]))

(defn clj-kondo-findings [input-string]
  ;; ensure latest hook is used
  (require '[clj-kondo.impl.hooks] :reload)
  (-> input-string
    (with-in-str
      (clj-kondo/run!
        {:lint  ["-"]
         :cache false
         :config
         '{:hooks   {:analyze-call
                     {com.fulcrologic.guardrails.core/>defn
                      com.fulcrologic.guardrails.clj-kondo-hooks/>defn
                      com.fulcrologic.guardrails.core/>defn-
                      com.fulcrologic.guardrails.clj-kondo-hooks/>defn}}
           :linters {:clj-kondo.fulcro.>defn/invalid-gspec {:level :error}}
           :lint-as {com.fulcrologic.guardrails.core/>def clojure.spec.alpha/def}}}))
    (:findings)))

(deftest >defn-hook-happy-path
  (is (= []
        (clj-kondo-findings
          "(ns foo
  (:require
    [com.fulcrologic.guardrails.core :refer [>defn]]
    [clojure.spec.alpha :as s]))

(>defn simples
  [a]
  [int? => int?]
  (if (> a 0)
    (* a (dec a))
    1))

(>defn one-list-arity
  ([b]
   [int? => int?]
   (if (> b 0)
     (* b (dec b))
     1)))

(>defn docstring
  \"docstring\"
  [a]
  [int? => int?]
  (if (> a 0)
    (* a (dec a))
    1))

(>defn docstring-one-list-arity
  \"docstring\"
  ([a]
   [int? => int?]
   (if (> a 0)
     (* a (dec a))
     1)))

(>defn test-function
  \"docstring\"
  ([a]
   [int? => int?]
   (if (> a 0)
     (* a (test-function (dec a)))
     1))
  ([a b]
   [int? int? => int?]
   (if (> a b)
     (recur a (inc b))
     (+ a b)))
  ([a b c & d]
   [int? int? int? (s/* int?) => '=> int?]
   (if (seq d)
     (reduce + 0 d)
     (+ a b c))))
"))))

(deftest >defn-hook-errors
  (is (= [{:end-row  8
           :type     :clj-kondo.fulcro.>defn/invalid-gspec
           :level    :error
           :filename "<stdin>"
           :col      3
           :end-col  27
           :langs    ()
           :message  "Gspec requires exactly one `=>` or `:ret`"
           :row      8}
          {:end-row  15
           :type     :clj-kondo.fulcro.>defn/invalid-gspec
           :level    :error
           :filename "<stdin>"
           :col      4
           :end-col  10
           :langs    ()
           :message  "Gspec requires exactly one `=>` or `:ret`"
           :row      15}
          {:end-row  23
           :type     :clj-kondo.fulcro.>defn/invalid-gspec
           :level    :error
           :filename "<stdin>"
           :col      4
           :end-col  28
           :langs    ()
           :message  "Gspec requires exactly one `=>` or `:ret`"
           :row      23}
          {:end-row  23
           :type     :unresolved-symbol
           :level    :error
           :filename "<stdin>"
           :col      10
           :end-col  22
           :langs    ()
           :message  "Unresolved symbol: =wrong-sym=>"
           :row      23}]
        (clj-kondo-findings
          "(ns foo
  (:require
    [com.fulcrologic.guardrails.core :refer [>defn]]
    [clojure.spec.alpha :as s]))

(>defn simples
  [a]
  [int? => int? :ret int?]
  (if (> a 0)
    (* a (dec a))
    1))

(>defn one-list-arity
  ([b]
   [int?]
   (if (> b 0)
     (* b (dec b))
     1)))

(>defn test-function
  \"docstring\"
  ([a]
   [int? =wrong-sym=> int?]
   (if (> a 0)
     (* a (test-function (dec a)))
     1))
  ([a b]
   [int? int? => int?]
   (if (> a b)
     (recur a (inc b))
     (+ a b)))
  ([a b c & d]
   [int? int? int? (s/* int?) => int?]
   (if (seq d)
     (reduce + 0 d)
     (+ a b c))))
"))))

(defn- defprotocol-findings [input-string]
  (require '[clj-kondo.impl.hooks] :reload)
  (->> (with-in-str input-string
         (clj-kondo/run!
           {:lint   ["-"]
            :cache  false
            :config '{:hooks   {:analyze-call
                                {com.fulcrologic.guardrails.core/>defprotocol
                                 com.fulcrologic.guardrails.clj-kondo-hooks/>defprotocol
                                 com.fulcrologic.guardrails.malli.core/>defprotocol
                                 com.fulcrologic.guardrails.clj-kondo-hooks/>defprotocol}}
                      :linters {:clj-kondo.fulcro.>defn/invalid-gspec {:level :error}}}}))
    (:findings)
    (mapv #(select-keys % [:row :type :message]))))

(deftest >defprotocol-hook-happy-path
  (is (= []
        (defprotocol-findings
          "(ns foo
  (:require
    [com.fulcrologic.guardrails.core :refer [>defprotocol]]
    [com.fulcrologic.guardrails.malli.core :as grm]
    [clojure.spec.alpha :as s]))

(>defprotocol Foo
  \"doc\"
  :extend-via-metadata true
  (bar [::Foo string? => string?] [::Foo string? int? | #(pos? arg2) => string?] \"bar doc\")
  (baz [::Foo => (s/nilable int?)]))

(grm/>defprotocol MFoo
  (mbar [::MFoo :string => :string]))

(defrecord R [] Foo (bar [_ s] s) (bar [_ s _n] s) (baz [_] 1))

(defn uses [x]
  [(bar x \"a\") (bar x \"a\" 1) (baz x) (mbar x \"s\") (->R)])
"))))

(deftest >defprotocol-hook-errors
  (is (= [{:row 8 :type :invalid-arity :message "foo/bar is called with 1 arg but expects 2 or 3"}
          {:row 8 :type :invalid-arity :message "foo/baz is called with 3 args but expects 1"}
          {:row 11 :type :clj-kondo.fulcro.>defn/invalid-gspec :message "Gspec requires exactly one `=>` or `:ret`"}
          {:row 12 :type :clj-kondo.fulcro.>defn/invalid-gspec :message "Duplicate gspec for arity 1."}
          {:row 13 :type :clj-kondo.fulcro.>defn/invalid-gspec
           :message "Protocol method gspec needs at least one argument spec (for `this`)."}]
        (defprotocol-findings
          "(ns foo
  (:require [com.fulcrologic.guardrails.core :refer [>defprotocol]]))

(>defprotocol Foo
  (bar [::Foo string? => string?] [::Foo string? int? => string?])
  (baz [::Foo => int?]))

(defn uses [x] [(bar x) (baz x 1 2)])

(>defprotocol Bad
  (q [::Bad string?])
  (r [::Bad => int?] [::Bad => int?])
  (t [=> int?]))
"))))

(deftest >defprotocol-hook-variadic
  (is (= [{:row 4 :type :clj-kondo.fulcro.>defn/invalid-gspec :message "Variadic gspecs are not supported in >defprotocol."}]
        (defprotocol-findings
          "(ns foo
  (:require [com.fulcrologic.guardrails.core :refer [>defprotocol]]))

(>defprotocol E (e1 [::E int? & int? => int?]))

(defn uses [x] (e1 x 1 2))
"))))
