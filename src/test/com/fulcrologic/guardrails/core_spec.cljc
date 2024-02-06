(ns com.fulcrologic.guardrails.core-spec
  (:require
    [clojure.spec.alpha :as s]
    [com.fulcrologic.guardrails.config :as config]
    [com.fulcrologic.guardrails.core :as gr :refer [=> >defn]]
    [fulcro-spec.core :refer [=throws=> assertions provided specification]]))

#?(:clj
   (do
     (System/setProperty "guardrails.enabled" "")
     (System/setProperty "guardrails.config" "guardrails-test.edn")))

#?(:clj
   (specification "loading config"
     (assertions
       "loads the config from the disk file"
       (config/get-env-config false) => {:defn-macro             nil
                                         :throw?                 true
                                         :guardrails/compact?    true,
                                         :guardrails/trace?      true,
                                         :guardrails/stack-trace :prune,
                                         :expound                {:show-valid-values? true
                                                                  :print-specs?       true}})))

#?(:clj
   (specification "Normal mode >defn macro"
     (let [test-fn '(>defn f [x] [int? => int?] (inc x))]

       (provided "There is no config"
         (config/get-env-config) => nil
         (let [output (gr/>defn* {} test-fn (rest test-fn) {})]
           (assertions
             "Emits a normal function"
             output => '(defn f [x] (inc x)))))

       (provided "There is a free config"
         (config/get-env-config & _) => {}
         (gr/generate-defn & _) => '(:stub/free-defn)

         (let [output (gr/>defn* {} test-fn (rest test-fn) {})]
           (assertions
             "Emits the free version"
             output => `(:stub/free-defn)))))))

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
   [int? int? int? (s/* int?) => int?]
   (if (seq d)
     (reduce + 0 d)
     (+ a b c))))

(s/def ::a int?)
(s/def ::b int?)

(>defn kw-func
  [& {:keys [a b] :as c}]
  [(s/keys* :req-un [::a] :opt-un [::b]) => int?]
  (+ a b))

(>defn kw-func2
  [x & {:keys [a b] :as c}]
  [int? (s/keys* :req-un [::a] :opt-un [::b]) => int?]
  (+ x a b))

(>defn seq-func
  [x & [a b & more]]
  [int? (s/* int?) => int?]
  (+ x a b))

(>defn vararg-seq [& targets]
  [(s/* vector?) => vector?]
  (into [] (seq targets)))

(specification "General transformed functions"
  (assertions
    "fixed arity, recursive"
    (test-function 3) => 6
    "Fixed-arity with bad argument"
    (test-function 9.7) =throws=> #"should satisfy"
    "fixed arity, tail recusive"
    (test-function 3 2) => 6
    "(fails on bad args)"
    (test-function 5 9.7) =throws=> #"should satisfy"
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
