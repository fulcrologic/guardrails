(ns com.fulcrologic.guardrails.noop
  "
  EXPERIMENTAL.

  Same interface as `core`, but the items in this namespace are no-ops. This is useful for
  making cljs builds smaller by eliding spec internment that cannot be dead-code eliminated.

  You can use ns aliasing in shadow-cljs (or stock cljs compiler) config to alias any requires
  of `com.fulcrologic.guardrails.core` to this ns.

  WARNING: Make sure you don't need to specs at runtime for anything. Use `s/def` for those, and
  `>fdef` for ones that you want to disappear at runtime."
  #?(:cljs (:require-macros com.fulcrologic.guardrails.noop)))

(def => :ret)
(def | :st)
(def <- :gen)

#?(:clj
   (defmacro ? [& forms]))

#?(:clj (defmacro >defn
   [& forms]
   `(defn ~@forms)))

#?(:clj (defmacro >defn- [& forms] `(defn- ~@forms)))

#?(:clj (defmacro >def
   ([k spec-form])
   ([k _doc spec-form])))

#?(:clj (defmacro >fdef [& forms]))

;; TODO: clean >fn (no gspec)
#?(:clj (defmacro >fn [& forms] `(fn ~@forms)))

#?(:clj (defmacro >fspec [& forms]))
