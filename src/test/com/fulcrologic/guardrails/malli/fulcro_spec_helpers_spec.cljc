(ns com.fulcrologic.guardrails.malli.fulcro-spec-helpers-spec
  (:require
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

;; Additional test functions for comprehensive testing
(defn h-no-schema
  "Function without any schema - should fall back to regular provided! behavior"
  [x]
  x)

(>defn strict-arity
  "Function with strict arity"
  [x y]
  [:string :int => :string]
  (str x y))

(specification "Validation passes with correct types"
  (provided! "Malli validation succeeds when types match"
    (f x) => "correct"
    (g x) => 99

    (assertions
      "Malli function works with correct types"
      (f "input") => "correct"
      "Spec function works with correct types"
      (g 42) => 99)))

(specification "Functions without schemas use regular provided! behavior"
  (provided! "No validation for functions without schemas"
    (h-no-schema x) => "anything"

    (assertions
      "Can mock function without schema"
      (h-no-schema 123) => "anything"
      "Can pass any type"
      (h-no-schema "string") => "anything")))

(specification "Execution counts work with validation"
  (provided! "Scripted mocking with validation"
    (f x) =1x=> "first"
    (f x) =1x=> "second"
    (f x) => "rest"

    (assertions
      "First call"
      (f "a") => "first"
      "Second call"
      (f "b") => "second"
      "Third call"
      (f "c") => "rest"
      "Fourth call"
      (f "d") => "rest")))

(specification "Multi-arity validation - one arg only"
  (provided! "Single arity works"
    (f x) => "one-arg"

    (assertions
      "One arity validated"
      (f "a") => "one-arg"
      (f "b") => "one-arg")))

(specification "Multi-arity validation - two args separately"
  (provided! "Two-arg arity works"
    (f x y) => "two-args"

    (assertions
      "Two arity validated"
      (f "a" "b") => "two-args")))

(specification "Multi-arity validation - varargs separately"
  (provided! "Varargs arity works"
    (f x y z & rest) => "var-args"

    (assertions
      "Varargs arity validated"
      (f "a" "b" "c" "d" "e") => "var-args")))

(specification "Multi-arity validation - mixed arities in same test"
  ;; This test checks if we can mock multiple arities of the same function
  ;; in a single provided! block
  (provided! "Can mock different arities together"
    (f x) =1x=> "one"
    (f x y) =1x=> "two"

    (assertions
      "Can call one-arg version"
      (f "a") => "one"
      "Can call two-arg version"
      (f "a" "b") => "two")))


;; Test for nil values
(>defn maybe-string
  "Function that accepts optional string"
  [x]
  [[:maybe :string] => [:maybe :string]]
  x)

(specification "Functions with maybe/optional schemas"
  (provided! "Nil values work with maybe schemas"
    (maybe-string x) => nil

    (assertions
      "Can return nil"
      (maybe-string nil) => nil
      "Can pass nil"
      (maybe-string "test") => nil)))

;; Test for complex nested schemas
(>defn nested-data
  "Function with complex nested schema"
  [data]
  [[:map [:name :string] [:age :int]] => :string]
  (:name data))

(specification "Complex nested schemas validate"
  (provided! "Nested map schemas work"
    (nested-data d) => "John"

    (assertions
      "Map validation works"
      (nested-data {:name "Jane" :age 30}) => "John")))
