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
                 (when (and ast-node (not macro?))
                   {::gr.reg/extern-name  `(quote ~(:name ast-node))
                    ::gr.reg/extern-value s})))
             (clojure-resolve []
               (if (contains? env s)
                 {::gr.reg/extern-name  `(quote ~s)
                  ::gr.reg/extern-value s}
                 (let [sym-var (ns-resolve *ns* env s)
                       cls?    (class? sym-var)
                       macro?  (boolean (some-> sym-var meta :macro))]
                   (when (and sym-var (not cls?) (not macro?))
                     {::gr.reg/extern-name  `(quote ~(symbol sym-var))
                      ::gr.reg/extern-value (symbol sym-var)
                      ::gr.reg/class?       cls?}))))]
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
