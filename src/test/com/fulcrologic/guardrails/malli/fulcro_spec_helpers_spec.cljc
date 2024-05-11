(ns com.fulcrologic.guardrails.malli.fulcro-spec-helpers-spec
  (:require
    [com.fulcrologic.guardrails.malli.core :refer [>defn =>]]
    [com.fulcrologic.guardrails.malli.fulcro-spec-helpers :refer [provided! when-mocking!]]
    [fulcro-spec.core :refer [specification assertions =1x=>]]))

(>defn f
  ([x] [:string => :string] x)
  ([x y] [:string :string => :string] y)
  ([x y z & more] [:string :string :string [:* :string] => :string] z))

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

(specification "When mocking argument validation"
  (when-mocking!
    (f x) => "hello"

    (try
      (f 5)
      (catch #?(:clj Throwable :cljs :default) t
        (assertions
          "Mocking catches errors on return value"
          (contains? (ex-data t) :problems) => true))))

  (provided! "I return the wrong kind of value"
    (f x) => "hello"

    (try
      (f 5)
      (catch #?(:clj Throwable :cljs :default) t
        (assertions
          "Mocking notices it"
          (contains? (ex-data t) :problems) => true))))

  (provided! "I screw up varargs"
    (f x y z & rs) => "hello"

    (try
      (f "a" "b" "c" "d" "e" 99)
      (catch #?(:clj Throwable :cljs :default) t
        (assertions
          "Mocking notices it"
          (contains? (ex-data t) :problems) => true)))))

(specification "When mocking return value validation"
  (when-mocking!
    (f x) => 42

    (try
      (f "hello")
      (catch #?(:clj Throwable :cljs :default) t
        (assertions
          "Mocking catches errors on return value"
          (contains? (ex-data t) :problems) => true))))

  (provided! "I return the wrong kind of value"
    (f x) => 42

    (try
      (f "hello")
      (catch #?(:clj Throwable :cljs :default) t
        (assertions
          "Mocking notices it"
          (contains? (ex-data t) :problems) => true)))))
