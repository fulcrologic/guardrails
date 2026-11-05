(ns com.fulcrologic.guardrails.defprotocol-fixtures
  "Protocols defined with `>defprotocol` (spec and Malli flavor), implemented in every supported style. Shared by the
   CLJ and CLJS tests in `defprotocol-spec`."
  (:require
   [clojure.string :as str]
   [com.fulcrologic.guardrails.core :as gr]
   [com.fulcrologic.guardrails.malli.core :as grm]))

;; Guardrails must be enabled when this namespace is compiled (as for any other `>defn` test namespace).
#?(:clj
   (do
     (System/setProperty "guardrails.enabled" "")
     (System/setProperty "guardrails.config" "guardrails-test.edn")))

(gr/>defprotocol Foo
  "Spec flavor."
  :extend-via-metadata true
  (bar [::Foo string? => string?] [::Foo string? int? => string?] "bar doc")
  (baz [::Foo => int?] "baz doc"))

(grm/>defprotocol MFoo
  "Malli flavor."
  :extend-via-metadata true
  (mbar [::MFoo :string => :string] [::MFoo :string :int => :string] "mbar doc")
  (mbaz [::MFoo => :int] "mbaz doc"))

;; ^:redef so that the tests can with-redefs this helper under direct linking.
(defn ^:redef out
  "Returns `tag:s`, or the (invalid) number 42 when `s` starts with \"bad-ret\"."
  [tag s]
  (if (str/starts-with? s "bad-ret") 42 (str tag ":" s)))

;; extend-type on a deftype
(deftype DT [])
(extend-type DT
  Foo
  (bar ([_ s] (out "DT" s)) ([_ s n] (out "DT" (str s n))))
  (baz [_] 1)
  MFoo
  (mbar ([_ s] (out "DT" s)) ([_ s n] (out "DT" (str s n))))
  (mbaz [_] 1))

;; inline defrecord (baz self-calls bar on `this`, whose tag is Rec)
(defrecord Rec [v]
  Foo
  (bar [_ s] (out "Rec" s))
  (bar [_ s n] (out "Rec" (str s n)))
  (baz [this] (count (bar this "self")))
  MFoo
  (mbar [_ s] (out "Rec" s))
  (mbar [_ s n] (out "Rec" (str s n)))
  (mbaz [this] (count (mbar this "self"))))

;; inline deftype
(deftype IT []
  Foo
  (bar [_ s] (out "IT" s))
  (bar [_ s n] (out "IT" (str s n)))
  (baz [_] 3)
  MFoo
  (mbar [_ s] (out "IT" s))
  (mbar [_ s n] (out "IT" (str s n)))
  (mbaz [_] 3))

(defn a-reify []
  (reify
    Foo
    (bar [_ s] (out "reify" s))
    (bar [_ s n] (out "reify" (str s n)))
    (baz [_] 4)
    MFoo
    (mbar [_ s] (out "reify" s))
    (mbar [_ s n] (out "reify" (str s n)))
    (mbaz [_] 4)))

;; string, number, nil, Object/default
(extend-protocol Foo
  #?(:clj String :cljs string)
  (bar ([this s] (out (str "string(" this ")") s)) ([_ s n] (out "string" (str s n))))
  (baz [this] (count this))
  #?(:clj Long :cljs number)
  (bar ([_ s] (out "number" s)) ([_ s n] (out "number" (str s n))))
  (baz [this] (int this))
  nil
  (bar ([_ s] (out "nil" s)) ([_ s n] (out "nil" (str s n))))
  (baz [_] 0)
  #?(:clj Object :cljs default)
  (bar ([_ s] (out "default" s)) ([_ s n] (out "default" (str s n))))
  (baz [_] -1))

(extend-protocol MFoo
  #?(:clj String :cljs string)
  (mbar ([this s] (out (str "string(" this ")") s)) ([_ s n] (out "string" (str s n))))
  (mbaz [this] (count this))
  #?(:clj Long :cljs number)
  (mbar ([_ s] (out "number" s)) ([_ s n] (out "number" (str s n))))
  (mbaz [this] (int this))
  nil
  (mbar ([_ s] (out "nil" s)) ([_ s n] (out "nil" (str s n))))
  (mbaz [_] 0)
  #?(:clj Object :cljs default)
  (mbar ([_ s] (out "default" s)) ([_ s n] (out "default" (str s n))))
  (mbaz [_] -1))

;; CLJ only: extend with a keyword map
#?(:clj (deftype MapT []))
#?(:clj
   (extend MapT
     Foo {:bar (fn ([_ s] (out "map" s)) ([_ s n] (out "map" (str s n))))
          :baz (fn [_] 6)}
     MFoo {:mbar (fn ([_ s] (out "map" s)) ([_ s n] (out "map" (str s n))))
           :mbaz (fn [_] 6)}))

;; CLJS only: specify!
#?(:cljs
   (defn a-specify []
     (specify! #js {}
       Foo
       (bar ([_ s] (out "specify" s)) ([_ s n] (out "specify" (str s n))))
       (baz [_] 5)
       MFoo
       (mbar ([_ s] (out "specify" s)) ([_ s n] (out "specify" (str s n))))
       (mbaz [_] 5))))

;; :extend-via-metadata
(defn a-meta []
  (with-meta {}
    {`bar  (fn ([_ s] (out "meta" s)) ([_ s n] (out "meta" (str s n))))
     `baz  (fn [_] 7)
     `mbar (fn ([_ s] (out "meta" s)) ([_ s n] (out "meta" (str s n))))
     `mbaz (fn [_] 7)}))

;; Not a top-level form: the method vars do not exist when the expansion is compiled.
(when true
  (gr/>defprotocol Nested
    (nested [::Nested string? => string?])))

(extend-protocol Nested
  #?(:clj String :cljs string)
  (nested [this s] (out this s)))

;; Only guardrails config (here `:throw? false`) flows from the protocol name to the methods, not e.g. `:private`.
(grm/>defprotocol ^:private ^{:no-doc true :throw? false} MetaP
  (mp [::MetaP => :int]))

(extend-protocol MetaP
  #?(:clj String :cljs string)
  (mp [this] this))
