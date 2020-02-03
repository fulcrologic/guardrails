;; Copyright (c) George Lipov. All rights reserved.
;; Additional code Copyright Fulcrologic, LLC.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns com.fulcrologic.guardrails.core
  #?(:cljs (:require-macros com.fulcrologic.guardrails.core))
  (:require
    #?@(:clj [[clojure.set :refer [union difference map-invert]]
              [clojure.walk :as walk]
              [com.fulcrologic.guardrails.utils :refer [cljs-env? clj->cljs]]
              [com.fulcrologic.guardrails.config :as cfg]])
    [clojure.string :as string]
    [taoensso.timbre :as log]
    [clojure.spec.alpha :as s]
    [expound.alpha :as exp]))

;; It doesn't actually matter what these are bound to, they are stripped by
;; the macros they're used in and never end up in the final code. This is just
;; so they can be used without '=> cannot be resolved' errors in the IDE.
(def => :ret)
(def | :st)
(def <- :gen)

;; runtime checking (both clj and cljs
(defn- output-fn [data]
  (let [{:keys [level ?err msg_ ?ns-str ?file hostname_
                timestamp_ ?line]} data]
    (str
      (string/upper-case (name level)) " "
      (force msg_)
      (when-let [err ?err]
        (str "\n" (log/stacktrace err {}))))))

(defn run-check [args? {:keys [log-level vararg? throw? fn-name]} spec value]
  (let [vargs?          (and args? vararg?)
        varg            (if vargs? (last (seq value)) nil)
        specable-args   (if vargs?
                          (if (map? varg) (into (vec (butlast value)) (flatten (seq varg))) (into (vec (butlast value)) (seq varg)))
                          value)
        valid-exception (atom nil)]
    (try
      (when-not (s/valid? spec specable-args)
        (let [config  (assoc log/*config* :output-fn output-fn)
              problem (exp/expound-str spec specable-args)]
          (log/log* config (or log-level :error) (str fn-name (if args? " argument list" " return type") "\n") problem)
          (when throw?
            (reset! valid-exception (ex-info problem {:fn-name (str fn-name)})))))
      (catch #?(:cljs :default :clj Throwable) e
        (log/error e "BUG: Internal error in expound or clojure spec.")))
    (when @valid-exception
      (throw @valid-exception)))
  nil)


#?(:clj
   (defn clean-defn
     "This removes the gspec and returns a
     clean defn for use in production builds."
     [op forms]
     (let [single-arity? (fn [fn-forms] (boolean (some vector? fn-forms)))
           strip-gspec   (fn [body] (let [[args _gspec & more] body]
                                      (cons args more)))]
       (->> (if (single-arity? forms)
              (let [[head-forms body-forms] (split-with (complement vector?) forms)]
                `(~op ~@head-forms ~@(strip-gspec body-forms)))
              (let [[head-forms body-forms tail-attr-map] (partition-by (complement seq?) forms)]
                `(~op ~@head-forms ~@(map strip-gspec body-forms) ~@tail-attr-map)))
         (remove nil?)))))

#?(:clj
   (defn- count-args
     "Returns a tuple with the number of regular and non-variadic arguments."
     [conformed-args]
     [(count (:args conformed-args))
      (if (:varargs conformed-args) 1 0)]))

#?(:clj
   (defmacro ? [& forms]
     (cond-> `(s/nilable ~@forms)
       (cljs-env? &env) clj->cljs)))

#?(:clj
   (do
     (s/def ::defn-macro string?)
     (s/def ::expound (s/map-of keyword? any?))
     (s/def ::throw? boolean?)
     (s/def ::emit-spec? boolean?)
     (s/def ::log-level #{:trace :debug :info :warn :error :fatal :report})

     (s/def ::guardrails-config
       (s/keys
         :opt-un [::defn-macro
                  ::expound
                  ::throw?
                  ::emit-spec?
                  ::log-level]))

     ;; These are lifted straight from clojure.core.specs.alpha, because it
     ;; didn't seem possible to access them directly in the original namespace.
     (s/def ::local-name (s/and simple-symbol? #(not= '& %)))

     ;; sequential destructuring
     (s/def ::seq-binding-form
       (s/and vector?
         (s/cat :elems (s/* ::binding-form)
           :rest (s/? (s/cat :amp #{'&} :form ::binding-form))
           :as (s/? (s/cat :as #{:as} :sym ::local-name)))))

     ;; map destructuring
     (s/def ::keys (s/coll-of ident? :kind vector?))
     (s/def ::syms (s/coll-of symbol? :kind vector?))
     (s/def ::strs (s/coll-of simple-symbol? :kind vector?))
     (s/def ::or (s/map-of simple-symbol? any?))
     (s/def ::as ::local-name)

     (s/def ::map-special-binding
       (s/keys :opt-un [::as ::or ::keys ::syms ::strs]))

     (s/def ::map-binding (s/tuple ::binding-form any?))

     (s/def ::ns-keys
       (s/tuple
         (s/and qualified-keyword? #(-> % name #{"keys" "syms"}))
         (s/coll-of simple-symbol? :kind vector?)))

     (s/def ::map-bindings
       (s/every (s/or :mb ::map-binding
                  :nsk ::ns-keys
                  :msb (s/tuple #{:as :or :keys :syms :strs} any?))
         :into {}))

     (s/def ::map-binding-form (s/merge ::map-bindings ::map-special-binding))

     (s/def ::binding-form
       (s/or :sym ::local-name
         :seq ::seq-binding-form
         :map ::map-binding-form))

     ;;; Function and >defn specs

     (s/def ::arg-list
       (s/and vector?
         (s/cat :args (s/* ::binding-form)
           :varargs (s/? (s/cat :amp #{'&} :form ::binding-form)))))

     (s/def ::pred-arg-list
       (s/and vector?
         (s/cat :args (s/* (s/or :sym ::local-name)))))

     (s/def ::anon-args+body
       (s/cat :args ::arg-list
         :body (s/* any?)))

     (s/def ::anon-fn
       (s/and seq?
         (s/cat :op #{'fn* 'fn}
           :name (s/? simple-symbol?)
           :bs (s/alt :arity-1 ::anon-args+body
                 :arity-n (s/+ (s/spec ::anon-args+body))))))

     (s/def ::pred-fn
       (s/and seq?
         (s/cat :op #{'fn* 'fn}
           :name (s/? simple-symbol?)
           :args ::pred-arg-list
           :body any?)))

     (s/def ::spec-elem
       (s/or :set set?
         :pred-sym (s/and symbol?
                     (complement #{'| '=>})
                     ;; REVIEW: should the `?` be a requirement?
                     #(string/ends-with? (str %) "?"))
         :gspec (s/or :nilable-gspec ::nilable-gspec :gspec ::gspec)
         :spec-key qualified-keyword?
         :fun ::pred-fn
         :list seq?))

     (s/def ::such-that-op #{:st '|})
     (s/def ::ret-op #{:ret '=>})
     (s/def ::gen-op #{:gen '<-})

     (s/def ::gspec
       (s/and vector?
         (s/cat :args (s/? (s/cat :args (s/+ ::spec-elem)
                             :args-such-that (s/? (s/cat :op ::such-that-op
                                                    :preds (s/+ ::pred-fn)))))
           :ret-op ::ret-op
           :ret ::spec-elem
           :fn-such-that (s/? (s/cat :op ::such-that-op
                                :preds (s/+ ::pred-fn)))
           :gen (s/? (s/cat :op ::gen-op
                       :gen-fn (s/? (some-fn seq? symbol?)))))))

     (s/def ::nilable-gspec
       (s/and vector?
         (s/cat :maybe #{'? 's/nilable}
           :gspec ::gspec)))

     (s/def ::prepost (s/map-of #{:pre :post}
                        (s/coll-of seq?
                          :kind vector?
                          :distinct true)))

     (s/def ::args+body
       (s/cat :args ::arg-list
         :body (s/alt :prepost+body (s/cat :prepost ::prepost
                                      :body (s/+ any?))
                 :body (s/* any?))))

     (s/def ::args+gspec+body
       (s/&
         (s/cat :args ::arg-list
           :gspec (s/nilable ::gspec)
           :body (s/alt :prepost+body (s/cat :prepost ::prepost
                                        :body (s/+ any?))
                   :body (s/* any?)))
         (fn arg-specs-match-param-count? [{:keys [args gspec]}]
           (if-not gspec
             true
             (let [argcount  (->> args count-args (apply +))
                   spec-args (:args gspec)]
               (if spec-args
                 (-> spec-args :args count (= argcount))
                 (= argcount 0)))))))


     (s/def ::defn
       (s/and seq?
         (s/cat :op #{'defn 'defn-}
           :name simple-symbol?
           :docstring (s/? string?)
           :meta (s/? map?)
           :bs (s/alt :arity-1 ::args+body
                 :arity-n (s/cat :bodies (s/+ (s/spec ::args+body))
                            :attr (s/? map?))))))

     ;;;; Main code generating functions

     (defn- unscrew-vec-unform
       "Half-arsed workaround for spec bugs CLJ-2003 and CLJ-2021."
       [unformed-arg]
       (if-not (sequential? unformed-arg)
         unformed-arg
         (let [malformed-seq-destructuring? (every-pred seq? (comp #{:as '&} first))
               [unformed malformed] (split-with (complement malformed-seq-destructuring?) unformed-arg)]
           (vec (concat unformed (apply concat malformed))))))


     (defn- gspec->fspec*
       [conformed-arg-list conformed-gspec anon-fspec? multi-arity-args? nilable?]
       (let [{argspec-def              :args
              retspec                  :ret
              fn-such-that             :fn-such-that
              {:keys [gen-fn] :as gen} :gen}
             conformed-gspec]
         (if (and anon-fspec?
               argspec-def
               (not gen)
               (some #{'any?} (-> argspec-def :args vals)))
           (if nilable? `(s/nilable ifn?) `ifn?)
           (let [extract-spec
                 (fn extract-spec [[spec-type spec]]
                   (if (= spec-type :gspec)
                     (if (= (key spec) :nilable-gspec)
                       (gspec->fspec* nil (-> spec val :gspec) true false true)
                       (gspec->fspec* nil (val spec) true false false))
                     spec))

                 named-conformed-args
                 (when argspec-def
                   (let [all-args     (remove nil? (concat (:args conformed-arg-list)
                                                     [(-> conformed-arg-list :varargs :form)]))
                         gen-arg-name (fn [index] (str "arg" (inc index)))
                         gen-name     (fn [index [arg-type arg :as full-arg]]
                                        (let [arg-name (if-not arg-type
                                                         (gen-arg-name index)
                                                         (case arg-type
                                                           :sym arg
                                                           :seq (or (-> arg :as :sym)
                                                                  (gen-arg-name index))
                                                           :map (or (-> arg :as)
                                                                  (gen-arg-name index))))]
                                          [(keyword arg-name) full-arg]))]
                     (map-indexed gen-name (or (seq all-args)
                                             (-> argspec-def :args count (repeat nil))))))

                 arg-binding-map
                 (if-not conformed-arg-list
                   {}
                   (if (every? #(= (-> % second key) :sym) named-conformed-args)
                     `{:keys ~(vec (map #(-> % first name symbol) named-conformed-args))}
                     (->> named-conformed-args
                       (map (fn [[arg-key conformed-arg]]
                              [(->> conformed-arg (s/unform ::binding-form) unscrew-vec-unform)
                               arg-key]))
                       (into {}))))

                 process-arg-pred
                 (fn process-arg-pred [{:keys [name args body]}]
                   (let [bindings (if-let [anon-arg (some-> args :args first second)]
                                    (assoc arg-binding-map :as anon-arg)
                                    arg-binding-map)]
                     (remove nil? `(fn ~name [~bindings] ~body))))

                 processed-args
                 (if-not argspec-def
                   `(s/cat)
                   (let [wrapped-params (->> argspec-def
                                          :args
                                          (map extract-spec)
                                          (interleave (map first named-conformed-args))
                                          (cons `s/cat))]
                     (if-let [args-such-that (:args-such-that argspec-def)]
                       (->> args-such-that
                         :preds
                         (map process-arg-pred)
                         (list* `s/and wrapped-params))
                       wrapped-params)))

                 process-ret-pred
                 (fn process-ret-pred [{:keys [name args body]}]
                   (let [anon-arg       (some-> args :args first second)
                         ret-sym        (gensym "ret__")
                         bindings       [{(if multi-arity-args?
                                            ['_ arg-binding-map]
                                            arg-binding-map) :args
                                          ret-sym            :ret}]
                         processed-body (if anon-arg
                                          (walk/postwalk-replace {anon-arg ret-sym} body)
                                          body)]
                     (remove nil? `(fn ~name ~bindings ~processed-body))))

                 fn-spec
                 (when fn-such-that
                   (let [processed-ret-preds (map process-ret-pred (:preds fn-such-that))]
                     (if (next processed-ret-preds)
                       (cons `s/and processed-ret-preds)
                       (first processed-ret-preds))))

                 final-fspec
                 (concat (when anon-fspec? [`s/fspec])
                   [:args processed-args]
                   [:ret (extract-spec retspec)]
                   (when fn-spec [:fn fn-spec])
                   (when gen-fn [:gen gen-fn]))]
             (if nilable? `(s/nilable ~final-fspec) final-fspec)))))



     ;; TODO make sure we check whether the variadic bodies are legit
     ;; Can not have more than one
     ;; Can not have one with more regular args than the variadic one
     ;; To what extent does the compiler already check this?
     (let [get-fspecs    (fn [fn-body]
                           (let [[param-count variadic] (-> fn-body :args count-args)
                                 gspec (or (:gspec fn-body)
                                         (s/conform ::gspec
                                           (vec (concat (repeat param-count 'any?)
                                                  (when (> variadic 0)
                                                    `[(s/* any?)])
                                                  '[=> any?]))))]
                             [(->> (if (> variadic 0) "n" param-count)
                                (str "arity-")
                                keyword)
                              (gspec->fspec* (:args fn-body) gspec false true false)]))
           get-spec-part (fn [part spec]
                           (->> spec
                             (drop-while (complement #{part}))
                             second))]
       (defn- generate-fspec-body [fn-bodies]
         (case (key fn-bodies)
           :arity-1
           (when-let [gspec (-> fn-bodies val :gspec)]
             (gspec->fspec* (-> fn-bodies val :args) gspec false false false))

           :arity-n
           (when (some :gspec (val fn-bodies))
             (let [fspecs           (map get-fspecs (val fn-bodies))
                   arg-specs        (mapcat (fn [[arity spec]]
                                              [arity (or (get-spec-part :args spec) `empty?)])
                                      fspecs)
                   fn-param         (gensym "p1__")
                   multi-ret-specs  (when (->> fspecs
                                            (map #(get-spec-part :ret (second %)))
                                            distinct
                                            count
                                            (not= 1))
                                      (mapcat (fn [[arity spec]]
                                                [arity `(s/valid? ~(get-spec-part :ret spec)
                                                          (:ret ~fn-param))])
                                        fspecs))
                   get-fn-clause    (partial get-spec-part :fn)
                   fn-specs         (when (->> fspecs (map second) (some get-fn-clause))
                                      (mapcat (fn [[arity spec]]
                                                [arity (if-let [fn-spec (get-fn-clause spec)]
                                                         `(s/valid? ~fn-spec ~fn-param)
                                                         true)])
                                        fspecs))
                   ;; NOTE: destructure args and ret in the arg vec
                   multi-ret-clause (when multi-ret-specs
                                      `(fn ~'valid-multi-arity-ret? [~fn-param]
                                         (case (-> ~fn-param :args key)
                                           ~@multi-ret-specs)))
                   multi-fn-clause  (when fn-specs
                                      `(fn ~'valid-multi-arity-fn? [~fn-param]
                                         (case (-> ~fn-param :args key)
                                           ~@fn-specs)))]
               ;; Using s/or here even though s/alt seems to be more common
               ;; for multi-arity specs in the wild. The spec error reporting
               ;; is much better and it's immediately clear what didn't match.
               (concat [:args `(s/or ~@arg-specs)]
                 (when-not multi-ret-clause
                   [:ret (get-spec-part :ret (-> fspecs first second))])
                 (when (or multi-ret-clause multi-fn-clause)
                   [:fn (if multi-fn-clause
                          (if multi-ret-clause
                            `(s/and ~multi-ret-clause ~multi-fn-clause)
                            multi-fn-clause)
                          multi-ret-clause)])))))))

     (def ^:private spec-op->type
       (let [map-prot     "cljs.core.IMap"
             coll-prot    "cljs.core.ICollection"
             ;; Needed because Closure compiler/JS doesn't consider strings seqable
             seqable-prot "(cljs.core.ISeqable|string)"]
         {'number?      "number"
          'integer?     "number"
          'int?         "number"
          'nat-int?     "number"
          'pos-int?     "number"
          'neg-int?     "number"
          'float?       "number"
          'double?      "number"
          'int-in       "number"
          'double-in    "number"

          'string?      "string"

          'boolean?     "boolean"

          'keys         map-prot
          'map-of       map-prot
          'map?         map-prot
          'merge        map-prot

          'set?         "cljs.core.ISet"
          'vector?      "cljs.core.IVector"
          'tuple        "cljs.core.IVector"
          'seq?         "cljs.core.ISeq"
          'seqable?     seqable-prot
          'associative? "cljs.core.IAssociative"
          'atom?        "cljs.core.IAtom"

          'coll-of      coll-prot
          'every        coll-prot

          'keyword?     "cljs.core.Keyword"
          'ifn?         "cljs.core.IFn"
          'fn?          "Function"}))

     (declare get-gspec-type)

     (defn- get-type [recursive-call conformed-spec-elem]
       (let [[spec-type spec-def] conformed-spec-elem
             spec-op
             ;; REVIEW: This kinda wants to be a multi-method when it grows up.
             (case spec-type
               :list (let [op (-> spec-def first name symbol)]
                       (cond
                         (#{'nilable '?} op) (concat (->> spec-def
                                                       second
                                                       (s/conform ::spec-elem)
                                                       (get-type true))
                                               [::nilable])
                         (#{'* '+} op) (concat (->> spec-def
                                                 second
                                                 (s/conform ::spec-elem)
                                                 (get-type true))
                                         [::variadic])
                         (#{'and} op) [(-> spec-def second)] ; TODO
                         (#{'coll-of 'every} op) [(or (->> spec-def
                                                        (drop-while (complement #{:kind}))
                                                        second)
                                                    op)]
                         :else [op]))
               ;;TODO support (some-fn and (s/or
               :gspec (let [gspec-def (val spec-def)]
                        (if (= (key spec-def) :nilable-gspec)
                          [(get-gspec-type (:gspec gspec-def)) ::nilable]
                          [(get-gspec-type gspec-def)]))
               :pred-sym [spec-def]
               [nil])]
         (if recursive-call
           spec-op
           (if-let [js-type (spec-op->type (first spec-op))]
             (let [modifiers (set (rest spec-op))]
               (as-> js-type t
                 (str (if (::nilable modifiers) "?" "!") t)
                 (str (when (::variadic modifiers) "...") t)))
             "*"))))


     (defn- get-gspec-type [conformed-gspec]
       (let [argspec-def (:args conformed-gspec)
             args-jstype (if-not argspec-def
                           ""
                           (->> (-> conformed-gspec :args :args)
                             (map (partial get-type false))
                             (string/join ", ")))
             ret-jstype  (get-type false (:ret conformed-gspec))]
         (str "function(" args-jstype "): " ret-jstype)))

     (defn- generate-type-annotations [env conformed-bs]
       (when (cljs-env? env)
         (case (key conformed-bs)
           :arity-1 (when-let [gspec (-> conformed-bs val :gspec)]
                      {:jsdoc [(str "@type {" (get-gspec-type gspec) "}")]})
           ;; REVIEW: There doesn't seem to be a way to get valid annotations for args of
           ;; multi-arity functions and attempts to just annotate the return value(s) failed
           ;; as well. It wasn't possible to put together an annotation which was both
           ;; considered valid and resulted in a successful type check.
           :arity-n nil #_(when-let [ret-types (as-> (val conformed-bs) x
                                                 (map #(get-type false (-> % :gspec :ret)) x)
                                                 (distinct x)
                                                 (when (not-any? #{"*" "?"} x) x))]
                            {:jsdoc [(str "@return {" (string/join "|" ret-types) "}")]}))))

     (defn- generate-fdef
       [env forms]
       (let [{[type fn-name] :name bs :bs} (s/conform ::>fdef-args forms)]
         (case type
           :sym (let [fdef `(s/fdef ~fn-name ~@(generate-fspec-body bs))]
                  fdef)
           :key `(s/def ~fn-name (s/fspec ~@(generate-fspec-body bs))))))))

#?(:clj
   (do
     (defn- process-defn-body
       [cfg fspec args+gspec+body]
       (let [{:keys                                 [env fn-name]
              {:keys [throw? emit-spec? log-level]} :config} cfg
             {:keys [args body]} args+gspec+body
             [prepost orig-body-forms] (case (key body)
                                         :prepost+body [(-> body val :prepost)
                                                        (-> body val :body)]
                                         :body [nil (val body)])
             process-arg   (fn [[arg-type arg]]
                             (as-> arg arg
                               (case arg-type
                                 :sym [arg-type arg]
                                 :seq [arg-type (update arg :as #(or % {:as :as :sym (gensym "arg_")}))]
                                 :map [arg-type (update arg :as #(or % (gensym "arg_")))])))
             ;; NOTE: usage of extract-arg isn't elegant, there's duplication, refactor
             extract-arg   (fn [[arg-type arg]]
                             (case arg-type
                               :sym arg
                               :seq (get-in arg [:as :sym])
                               :map (:as arg)
                               nil))
             {:keys [file line]} (if (cljs-env? env)
                                   (meta fn-name)
                                   {:file #?(:clj *file* :cljs "N/A")
                                    :line (some-> env :form meta :line)})
             unform-arg    #(->> % (s/unform ::binding-form) unscrew-vec-unform)
             reg-args      (->> args :args (mapv process-arg))
             arg->sym      #(let [f (into {} [%])]
                              (or
                                (:sym f)
                                (some-> f :seq :as :sym)
                                (some-> f :map :as)))
             reg-arg-names (mapv arg->sym reg-args)
             var-arg       (some-> args :varargs :form process-arg)
             arg-list      (vec (concat (map unform-arg reg-args)
                                  (when var-arg ['& (unform-arg var-arg)])))
             sym-arg-list  (if var-arg
                             (conj reg-arg-names (arg->sym var-arg))
                             reg-arg-names)
             body-forms    orig-body-forms
             where         (str file ":" line " " fn-name "'s")
             argspec       (gensym "argspec")
             opts          {:fn-name   where :emit-spec? emit-spec?
                            :log-level log-level :throw? throw? :vararg? (boolean var-arg)}
             args-check    `(when ~argspec (run-check true ~opts ~argspec ~sym-arg-list))
             retspec       (gensym "retspec")
             ret           (gensym "ret")
             ret-check     `(when ~retspec (run-check false ~opts ~retspec ~ret))
             real-function `(fn ~arg-list ~@body-forms)
             f             (gensym "f")
             call          (if (boolean var-arg)
                             `(cond
                                (map? ~(last sym-arg-list)) (apply ~f ~@(butlast sym-arg-list) (apply concat (last ~sym-arg-list)))
                                (seq ~(last sym-arg-list)) (apply ~f ~@sym-arg-list)
                                :else (~f ~@(butlast sym-arg-list)))
                             `(~f ~@sym-arg-list))]
         `(~@(remove nil? [arg-list prepost])
            (let [{~argspec :args ~retspec :ret} ~fspec]
              ~args-check
              (let [~f ~real-function
                    ~ret ~call]
                ~ret-check
                ~ret)))))

     (defn- generate-defn
       [forms private env]
       (let [conformed-gdefn   (s/conform ::>defn-args forms)
             fn-bodies         (:bs conformed-gdefn)
             arity             (key fn-bodies)
             fn-name           (:name conformed-gdefn)
             docstring         (:docstring conformed-gdefn)
             meta-map          (merge (:meta conformed-gdefn)
                                 (generate-type-annotations env fn-bodies)
                                 {::guardrails true})
             ;;; Assemble the config
             {:keys [defn-macro emit-spec?] :as config} (cfg/merge-config env (meta fn-name) meta-map)
             defn-sym          (cond defn-macro (with-meta (symbol defn-macro) {:private private})
                                     private 'defn-
                                     :else 'defn)
             ;;; Code generation
             fdef-body         (generate-fspec-body fn-bodies)
             fdef              (when (and fdef-body emit-spec?) `(s/fdef ~fn-name ~@fdef-body))
             individual-arity-fspecs
                               (map (fn [{:keys [args gspec]}]
                                      (when gspec
                                        (gspec->fspec* args gspec true false false)))
                                 (val fn-bodies))

             process-fn-bodies (fn []
                                 (let [process-cfg {:env     env
                                                    :config  config
                                                    :fn-name fn-name}]
                                   (case arity
                                     :arity-1 (->> fn-bodies val (process-defn-body process-cfg `(s/fspec ~@fdef-body)))
                                     :arity-n (map (partial process-defn-body process-cfg)
                                                individual-arity-fspecs
                                                (val fn-bodies)))))
             main-defn         `(~@(remove nil? [defn-sym fn-name docstring meta-map])
                                  ~@(process-fn-bodies))]
         `(do ~fdef (declare ~fn-name) ~main-defn)))

     (defmacro emit-specs? []
       (get (cfg/get-env-config) :emit-spec? true))

     ;;;; Main macros and public API

     (s/def ::>defn-args
       (s/and seq?                                          ; REVIEW
         (s/cat :name simple-symbol?
           :docstring (s/? string?)
           :meta (s/? map?)
           :bs (s/alt :arity-1 ::args+gspec+body
                 ;; TODO: add tail-attr-map support after this
                 :arity-n (s/+ (s/and seq? ::args+gspec+body))))))



     (defmacro >defn
       "Like defn, but requires a (nilable) gspec definition and generates
       additional `s/fdef`, generative tests, instrumentation code, an
       fspec-based stub, and/or tracing code, depending on the configuration
       metadata and the existence of a valid gspec and non-nil body."
       {:arglists '([name doc-string? attr-map? [params*] gspec prepost-map? body?]
                    [name doc-string? attr-map? ([params*] gspec prepost-map? body?) + attr-map?])}
       [& forms]
       (if (cfg/get-env-config)
         (cond-> (remove nil? (generate-defn forms false (assoc &env :form &form)))
           (cljs-env? &env) clj->cljs)
         (clean-defn 'defn forms)))

     (s/fdef >defn :args ::>defn-args)


     ;; NOTE: lots of duplication - refactor this to set/pass ^:private differently and call >defn
     (defmacro >defn-
       "Like defn-, but requires a (nilable) gspec definition and generates
       additional `s/fdef`, generative tests, instrumentation code, an
       fspec-based stub, and/or tracing code, depending on the configuration
       metadata and the existence of a valid gspec and non-nil body."
       {:arglists '([name doc-string? attr-map? [params*] gspec prepost-map? body?]
                    [name doc-string? attr-map? ([params*] gspec prepost-map? body?) + attr-map?])}
       [& forms]
       (if (cfg/get-env-config)
         (cond-> (remove nil? (generate-defn forms true &env))
           (cljs-env? &env) clj->cljs)
         (clean-defn 'defn- forms)))

     (s/fdef >defn- :args ::>defn-args)

     (defmacro >def
       "Simple pass-through to `s/def`, except it strips the
       specs in production – use for data specs you don't need
       in production when you want to minimise your build size.

       You can optionally send a documentation string as the second parameter, this
       is intended to be informational for the code reader, currently this is not stored
       anywhere, meaning you can't access this string at runtime."
       ([k spec-form]
        (when (emit-specs?)
          (cond-> `(s/def ~k ~spec-form)
            (cljs-env? &env) clj->cljs)))
       ([k _doc spec-form]
        `(>def ~k ~spec-form)))

     (s/def ::>fdef-args
       (s/and seq?                                          ;REVIEW
         (s/cat :name (s/or :sym symbol? :key qualified-keyword?)
           :bs (s/alt :arity-1 ::args+gspec+body
                 :arity-n (s/+ (s/and seq? ::args+gspec+body))))))

     (defmacro >fdef
       "Defines an fspec using gspec syntax – pretty much a `>defn` without the body.

       `name` can be a symbol or a qualified keyword, depending on whether the
       fspec is meant to be registered as a top-level fspec (=> s/fdef fn-sym
       ...) or used in other specs (=> s/def ::spec-keyword (s/fspec ...)). "
       {:arglists '([name [params*] gspec]
                    [name ([params*] gspec) +])}
       [& forms]
       (when (emit-specs?)
         (cond-> (remove nil? (generate-fdef &env forms))
           (cljs-env? &env) clj->cljs)))

     (s/fdef >fdef :args ::>fdef-args)))
