(ns com.fulcrologic.guardrails.malli.core-spec
  (:require
    [com.fulcrologic.guardrails.config :as config]
    [com.fulcrologic.guardrails.malli.core :refer [=> >defn]]
    [fulcro-spec.core :refer [=throws=> assertions specification]]))

#?(:clj
   (do
     (System/setProperty "guardrails.enabled" "")
     (System/setProperty "guardrails.config" "guardrails-test.edn")))

#?(:clj
   (specification "loading config"
     (assertions
       "loads the config from the disk file"
       (config/get-env-config false) => {:defn-macro nil
                                         :throw?     true
                                         :expound    {:show-valid-values? true
                                                      :print-specs?       true}})))

(>defn test-function
  "docstring"
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
   [int? int? int? [:* int?] => int?]
   (if (seq d)
     (reduce + 0 d)
     (+ a b c))))

(>defn kw-func
  [& {:keys [a b] :as c}]
  [[:* [:cat :keyword :any]] => int?]
  (+ a b))

(>defn kw-func2
  [x & {:keys [a b] :as c}]
  [int? [:* [:cat :keyword :any]] => int?]
  (+ x a b))

(>defn seq-func
  [x & [a b & more]]
  [int? [:* int?] => int?]
  (+ x a b))

(>defn vararg-seq [& targets]
  [[:* vector?] => vector?]
  (into [] (seq targets)))

(specification "General transformed functions"
  (assertions
    "fixed arity, recursive"
    (test-function 3) => 6
    "Fixed-arity with bad argument"
    (test-function 9.7) =throws=> #"should be an int"
    "fixed arity, tail recusive"
    (test-function 3 2) => 6
    "(fails on bad args)"
    (test-function 5 9.7) =throws=> #"should be an int"
    "vararg with no extra args"
    (test-function 1 1 1) => 3
    "vararg with extra args"
    (test-function 1 1 1 1 1) => 2
    "kwargs"
    (kw-func :a 1 :b 2) => 3
    (kw-func2 100 :a 1 :b 2) => 103
    "seq destructuring on vararg"
    (seq-func 100 1 2) => 103
    "vararg of sequences"
    (vararg-seq [:a 1] [:b 2]) => [[:a 1] [:b 2]]))
