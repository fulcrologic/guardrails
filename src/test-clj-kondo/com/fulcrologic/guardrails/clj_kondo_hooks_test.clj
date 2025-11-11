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
