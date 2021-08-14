(ns com.fulcrologic.guardrails.impl.externs
  (:require
    #?@(:clj [[clojure.walk :as walk]])
    [com.fulcrologic.guardrails.registry :as gr.reg]
    [com.fulcrologic.guardrails.utils :as utils]))

(defonce externs-registry (atom {}))
(defonce spec-registry (atom {}))
(defonce function-registry (atom {}))
(defonce external-function-registry (atom {}))

#?(:clj (try (require 'cljs.analyzer.api) (catch Exception _)))

#?(:clj
   (defn compiling-cljs? [env]
     (and (:ns env) (utils/compiling-cljs?))))

#?(:clj
   (defn cljc-resolve [env s]
     (letfn [(cljs-resolve []
               (let [rslv     (some-> (find-ns 'cljs.analyzer.api) (ns-resolve 'resolve))
                     ast-node (when rslv (rslv env s))
                     macro?   (boolean (:macro ast-node))]
                 (when ast-node
                   (cond-> {::gr.reg/extern-name `(quote ~(:name ast-node))
                            ::gr.reg/macro?      macro?}
                     (not macro?) (assoc ::gr.reg/extern-value s)))))
             (clojure-resolve []
               (if (contains? env s)
                 {::gr.reg/extern-name  `(quote ~s)
                  ::gr.reg/extern-value s}
                 (let [sym-var (ns-resolve *ns* env s)
                       cls?    (class? sym-var)
                       macro?  (boolean (some-> sym-var meta :macro))]
                   (when (and sym-var (not cls?))
                     (cond-> {::gr.reg/extern-name `(quote ~(symbol sym-var))
                              ::gr.reg/macro?      macro?}
                       (not macro?)
                       (assoc ::gr.reg/extern-value (symbol sym-var)))))))]
       (if (compiling-cljs? env)
         (cljs-resolve)
         (clojure-resolve)))))

#?(:clj
   (defn extern-symbols [env body]
     (let [externs (atom {})
           record! (fn [x]
                     (when (symbol? x)
                       (when-let [extern (cljc-resolve env x)]
                         (swap! externs assoc `(quote ~x) extern)))
                     x)]
       (walk/postwalk record! body)
       @externs)))

(defn register-externs! [NS fn-name externs]
  (swap! externs-registry assoc-in [NS fn-name] externs))

(defn register-specs! [function]
  (swap! spec-registry merge (::gr.reg/spec-registry function)))

(defn clean-function [function]
  (-> function
    (update ::gr.reg/arities
      (partial utils/map-vals #(dissoc % ::gr.reg/body)))
    (dissoc ::gr.reg/spec-registry)))

(defn register-function! [NS fn-name function]
  (register-specs! function)
  (swap! function-registry assoc-in [NS fn-name]
    (clean-function function)))

(defn record-defn! [NS {:as function ::gr.reg/keys [fn-name]} externs]
  (register-externs! NS fn-name externs)
  (register-function! NS fn-name function))

(defn register-external-function! [{:as external-function ::gr.reg/keys [var-name]}]
  (swap! external-function-registry assoc var-name
    (clean-function external-function)))

(defn record-fdef! [external-function]
  (register-specs! external-function)
  (register-external-function! external-function))

(defn function-info
  "Returns the information known about the given qualified symbol (if it was declared with >defn in
  copilot mode, or has register a gspec on an external function) ."
  [qualified-symbol]
  (let [spc         (namespace qualified-symbol)
        simple-name (symbol (name qualified-symbol))]
    (or
      (get @external-function-registry qualified-symbol)
      (get-in @function-registry [spc simple-name]))))

(defn pure?
  "Returns true if the given fully-qualified symbol was declared with >defn and the arity (which is a number
   or :n) is marked as pure."
  [qualified-symbol arity]
  (boolean
    (let [info       (function-info qualified-symbol)
          has-arity? (boolean (get-in info [::gr.reg/arities arity]))
          {:keys [pure pure?]} (get-in info [::gr.reg/arities (if has-arity? arity :n) ::gr.reg/gspec ::gr.reg/metadata])]
      (or pure pure?))))

(defn spec-system
  "Returns the function spec system that was used in the type signature of the given symbol, or nil if that
   function isn't registered with guardrails."
  [qualified-symbol]
  (let [{::gr.reg/keys [spec-system] :as info} (function-info qualified-symbol)]
    (when info
      (or spec-system :org.clojure/spec1))))

(defn run-registry-function
  "Run the given function defined by the qualified-symbol if and only if that arity of the function is pure.

   qualified-symbol - The symbol of the function
   args - a vector of arguments to pass to it.

   Throws IllegalArgumentException if that function arity is not marked pure."
  [qualified-symbol args]
  (if (pure? qualified-symbol (count args))
    (let [{::gr.reg/keys [fn-ref]} (function-info qualified-symbol)]
      (apply fn-ref args))
    (throw (IllegalArgumentException. (str qualified-symbol " is not a pure function.")))))