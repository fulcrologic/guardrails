(ns com.fulcrologic.guardrails.malli.fulcro-spec-helpers-spec
  (:require
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [com.fulcrologic.guardrails.core :as grc]
    [com.fulcrologic.guardrails.malli.core :refer [=> >defn]]
    [com.fulcrologic.guardrails.malli.fulcro-spec-helpers :refer [provided! when-mocking!]]
    [fulcro-spec.core :refer [=1x=> assertions specification]]))

(>defn f
  ([x] [:string => :string] x)
  ([x y] [:string :string => :string] y)
  ([x y z & more] [:string :string :string [:* :string] => :string] z))

(grc/>defn g [x] [int? => int?] x)

(specification "Clojure-spec based functions"
  (when-mocking!
    (g x) => 99

    (assertions
      "Mocking works"
      (g 10) => 99)))

(specification "Correct mocking"
  (when-mocking!
    (f x) =1x=> "A"
    (f x y) =1x=> "B"
    (f x y z & rest) =1x=> "C"

    (assertions
      "Works on one arity"
      (f "X") => "A"
      "Works on two arity"
      (f "X" "Y") => "B"
      "Works on n-arity"
      (f "a" "b" "c" "d" "e") => "C")))

(specification "Mixed Malli and Spec function mocking"
  (when-mocking!
    (f x) => "malli-result"
    (g x) => 100

    (assertions
      "Can mock Malli function f"
      (f "test") => "malli-result"
      "Can mock Spec function g"
      (g 50) => 100))

  (provided! "Mixed validation works"
    (f x) => "result"
    (g x) => 200

    (assertions
      "Malli validation works for f"
      (f "input") => "result"
      "Spec validation works for g"
      (g 75) => 200)))

;; Reproduction test functions
(grc/>defn validate-permissions
  "Mock function with valid spec"
  [user-id permission]
  [int? keyword? => boolean?]
  true)

(grc/>defn load-user-profile
  "Second mock with valid spec"
  [user-id]
  [int? => map?]
  {:id user-id})

(grc/>defn perform-action
  "Function under test that catches exceptions from mocks"
  [user-id]
  [int? => [:maybe map?]]
  (try
    (when (validate-permissions user-id :read)
      (load-user-profile user-id))
    (catch #?(:clj Exception :cljs :default) e
      ;; Swallow the exception and return nil
      nil)))

(specification "Intentional mock exceptions propagate normally"
  ;; Test that when a mock intentionally throws (as part of test setup),
  ;; that exception propagates normally and is NOT tracked as a validation error
  (when-mocking!
    (load-user-profile user-id) =1x=> (throw (ex-info "Database error" {:type :db-error}))

    (try
      (perform-action 123)
      (catch #?(:clj Exception :cljs :default) e
        (assertions
          "Intentional exception propagates through"
          (ex-message e) => "Database error"
          (get (ex-data e) :type) => :db-error))))

  ;; Also test that intentional exceptions work with valid guardrails specs
  (when-mocking!
    (g x) =1x=> (throw (ex-info "Intentional failure" {:reason :test}))

    (try
      (g 42)
      (catch #?(:clj Exception :cljs :default) e
        (assertions
          "Intentional exception from spec'd function works"
          (ex-message e) => "Intentional failure"
          (get (ex-data e) :reason) => :test)))))
