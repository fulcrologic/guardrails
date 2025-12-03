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
    #?@(:clj  [[clojure.set :as set]
               [clojure.walk :as walk]
               [com.fulcrologic.guardrails.impl.pro :as gr.pro]
               [com.fulcrologic.guardrails.utils :as utils :refer [cljs-env? clj->cljs strip-colors]]]
        :cljs [[com.fulcrologic.guardrails.impl.externs]
               [com.fulcrologic.guardrails.utils :as utils :refer [strip-colors]]
               [goog.object :as gobj]])
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [com.fulcrologic.guardrails.config :as gr.cfg]
    [expound.alpha :as exp]))

;; It doesn't actually matter what these are bound to, they are stripped by
;; the macros they're used in and never end up in the final code. This is just
;; so they can be used without '=> cannot be resolved' errors in the IDE.
(def => :ret)
(def | :st)
(def <- :gen)

(defonce ^:private !global-context (atom (list)))

(defn enter-global-context!
  "Push a global context, accessible from all threads, onto a stack. Used to add
  information to what guardrails will report when a function failed a check."
  [ctx]
  (swap! !global-context (partial cons ctx)))

(defn leave-global-context!
  "Pops a global context (see `enter-global-context!`). Should be passed the
  same context that was pushed, although is not enforced, as it's only to be
  easily compatible with fulcro-spec's hooks API."
  [ctx]
  (swap! !global-context rest))

#?(:clj
   (defmacro with-global-context
     "Wraps the body with an enter and leave global context.
      Will always call leave as it uses a try finally block.
      See `enter-global-context!`."
     [ctx & body]
     `(do
        (enter-global-context! ~ctx)
        (try ~@body
             (finally
               (leave-global-context! ~ctx))))))

(defn- get-global-context [] (first @!global-context))

;; runtime checking (both clj and cljs
(defn- output-fn [data]
  (let [{:keys [level ?err msg_ ?ns-str ?file hostname_
                timestamp_ ?line]} data]
    (str
      (str/upper-case (name level)) " "
      (force msg_)
      (when-let [err ?err]
        (str "\n" (utils/stacktrace err))))))

#?(:clj
   (defn now-ms ^long []
     (System/currentTimeMillis))
   :cljs
   (defn now-ms []
     (inst-ms (js/Date.))))

(def tap (resolve 'tap>))

(defn humanize-spec [explain-data {:guardrails/keys [compact? args? fqnm] :as expound-options}]
  (if compact?
    (let [lines (into [(if args?
                         (str fqnm "'s arguments:")
                         (str fqnm "'s return:"))]
                  (->>
                    (str/split-lines
                      (with-out-str
                        ((exp/custom-printer expound-options) explain-data)))
                    (remove (fn [l]
                              (or
                                (str/includes? l "------")
                                (re-matches #"^Detected \d.*$" l)
                                (re-matches #"^\s*$" l))))))]
      (str "  " (str/join "\n  " lines)))
    (with-out-str
      ((exp/custom-printer expound-options) explain-data))))

(defn run-check [{:guardrails/keys [malli? compact? use-stderr? validate-fn explain-fn fqnm humanize-fn]
                  :keys            [tap>? args? vararg? humanize-opts throw? fn-name]
                  :as              options}
                 spec
                 value]
  (let [start           (now-ms)
        vargs?          (and args? vararg?)
        varg            (if vargs? (last (seq value)) nil)
        specable-args   (if vargs?
                          (if (map? varg)
                            (into (vec (butlast value)) (flatten (seq varg)))
                            (into (vec (butlast value)) (seq varg)))
                          value)
        valid-exception (atom nil)]
    ;; FIXME: @gnl Output should be:
    ;; For function a.b/c
    ;; expected: [int? double? => int?]
    ;; actual: [nil 2.0] => "hello"
    (try
      (when-not (validate-fn spec specable-args)
        (binding [*out* (if use-stderr? #?(:clj *err* :cljs *out*) *out*)]
          (let [explain-data  (explain-fn spec specable-args)
                explain-human (humanize-fn explain-data (assoc humanize-opts
                                                          :guardrails/compact? compact?
                                                          :guardrails/fqnm fqnm
                                                          :guardrails/args? args?))
                e             (ex-info "" {})
                description   (utils/problem-description
                                (str
                                  "\nGuardrails:\n"
                                  explain-human)
                                e options)
                context       (get-global-context)]
            (utils/record-failure fqnm e)
            (when (and tap tap>?)
              (tap #:com.fulcrologic.guardrails
                      {:fn-name       fqnm
                       :failure-point (if args? :args :ret)
                       :spec          spec
                       :explain-data  explain-data
                       :explain-human (strip-colors explain-human)}))
            (if throw?
              (reset! valid-exception
                (ex-info (cond->> description context
                           (str "\nContext: " context))
                  (with-meta
                    #:com.fulcrologic.guardrails
                            {:_/type        :com.fulcrologic.guardrails/validation-error
                             :fn-name       fn-name
                             :failure-point (if args? :args :ret)
                             :spec          spec
                             :context       context}
                    #:com.fulcrologic.guardrails
                            {:val specable-args})))
              (utils/report-problem description)))))
      (catch #?(:cljs :default :clj Throwable) e
        (utils/report-exception e (str "BUG: Internal error in " (if malli? "Malli.\n" "expound or clojure spec.\n"))))
      (finally
        (let [duration (- (now-ms) start)]
          (when (> duration 100)
            (utils/report-problem (str "WARNING: " fn-name " " (if args? "argument specs" "return spec") " took " duration "ms to run.")
              nil options)))))
    (when @valid-exception
      (throw @valid-exception)))
  nil)

#?(:clj
   (defn clean-defn
     "This removes the gspec and returns a clean defn for use in production
     builds."
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

     (s/def ::guardrails-config
       (s/keys
         :opt-un [::defn-macro
                  ::expound
                  ::throw?]))

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
       (s/or
         :set set?
         :pred-sym (s/and symbol?
                     (complement #{'| '=> '<-})
                     ;; REVIEW: should the `?` be a requirement?
                     #(str/ends-with? (str %) "?"))
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
                            :attr (s/? map?))))))))

;;;; Main code generating functions

#?(:clj
   (defn- gspec->fspec*
     "Generates a clojure.spec fspec or Malli schema from a spec-conformed
     gspec. Output varies depending on whether the generated spec or schema is
     meant for external consumption by clojure.spec or Malli tools, or for
     internal checking with Guardrails."
     [{:com.fulcrologic.guardrails.core/keys
       [conformed-reg-args conformed-var-arg arg-syms clean-arg-vec]
       :as processed-args}
      conformed-gspec
      {:com.fulcrologic.guardrails.core/keys
       [anon-fspec? multi-arity-args? nilable? malli? external-consumption?]
       :as opts}]
     (let [{argspec-def              :args
            retspec                  :ret
            ret-such-that            :fn-such-that
            {:keys [gen-fn] :as gen} :gen}
           conformed-gspec]
       ;; gnl: This functions as a graceful downgrade for some validation cases
       ;; with clojure.spec where an anonymous or nested gspec for a function
       ;; argument has neither specific enough types nor a generator.
       (if (and anon-fspec?
             argspec-def
             (not gen)
             (some #{'any? :any} (-> argspec-def :args vals)))
         (if nilable?
           (if malli? `[:maybe ifn?] `(s/nilable ifn?))
           `ifn?)
         (let [extract-spec
               (fn extract-spec [[spec-type spec]]
                 (if (= spec-type :gspec)
                   (if (= (key spec) :nilable-gspec)
                     (gspec->fspec*
                       nil
                       (-> spec val :gspec)
                       {::anon-fspec? true
                        ::nilable?    true
                        ::malli?      malli?})
                     (gspec->fspec*
                       nil
                       (val spec)
                       {::anon-fspec? true
                        ::malli?      malli?}))
                   spec))

               ;; Destructuring vector for the function arguments with added
               ;; bindings for anonymous destructurings (those without an `:as`
               ;; attribute). The goal is to ensure that the argument and return
               ;; such-that predicates defined below can directly reference all
               ;; argument bindings
               arg-destructuring
               (if (empty? clean-arg-vec)
                 (if malli? [] {})
                 (if (every? (fn [[type _arg]] (= type :sym))
                       (remove nil? (conj conformed-reg-args conformed-var-arg)))
                   (if malli?
                     arg-syms
                     `{:keys ~arg-syms})
                   (if malli?
                     clean-arg-vec
                     (->> (map (fn [destruct-binding arg-sym]
                                 [destruct-binding (keyword arg-sym)])
                            clean-arg-vec
                            arg-syms)
                       (into {})))))

               ;; Process an argument predicate to make sure all direct
               ;; references to the argument bindings are valid
               process-arg-pred
               (fn process-arg-pred [{:keys [name args body]}]
                 (let [bindings (if-let [anon-arg (some-> args :args first second)]
                                  (if malli?
                                    (into arg-destructuring [:as anon-arg])
                                    (assoc arg-destructuring :as anon-arg))
                                  arg-destructuring)
                       function (remove nil? `(fn ~name [~bindings] ~body))]
                   (if malli?
                     [:fn function]
                     function)))

               ;; Argument spec, and-wrapped with all such-that predicates and
               ;; ready to be used for validation
               processed-arg-spec
               (if-not argspec-def
                 (if malli? `[:catn] `(s/cat))
                 (let [wrapped-params (as-> argspec-def __
                                        (:args __)
                                        (map extract-spec __)
                                        (if malli?
                                          (if anon-fspec?
                                            __
                                            (map #(do [(keyword %1) %2]) arg-syms __))
                                          (if anon-fspec?
                                            (interleave
                                              (map-indexed
                                                (fn [^long index _]
                                                  (keyword (format "arg%s" (str (inc index)))))
                                                (repeat nil))
                                              __)
                                            (interleave (map keyword arg-syms) __)))
                                        (if malli?
                                          (if anon-fspec?
                                            (vec (cons `:cat __))
                                            (vec (cons `:catn __)))
                                          (cons `s/cat __)))]
                   (if-let [args-such-that (:args-such-that argspec-def)]
                     (as-> args-such-that __
                       (:preds __)
                       (map process-arg-pred __)
                       (if malli?
                         ;; REVIEW: Malli doesn't seem to support arg
                         ;; predicates in function schemas, so we just strip
                         ;; them when outputting Malli schemas, but we do still
                         ;; use them for Guardrails validation
                         (if external-consumption?
                           wrapped-params
                           (vec (list* :and wrapped-params __)))
                         (list* `s/and wrapped-params __)))
                     wrapped-params)))

               ;; Making sure references to the argument bindings and return
               ;; value in ret such-that predicates are legit
               process-ret-pred
               (fn process-ret-pred [{:keys [name args body]}]
                 (let [anon-arg       (some-> args :args first second)
                       ret-sym        (gensym "ret__")
                       bindings       [{(if multi-arity-args?
                                          ['_ arg-destructuring]
                                          arg-destructuring) :args
                                        ret-sym              :ret}]
                       processed-body (if anon-arg
                                        (walk/postwalk-replace {anon-arg ret-sym} body)
                                        body)]
                   (remove nil? `(fn ~name ~bindings ~processed-body))))

               processed-ret-preds
               (when ret-such-that
                 (map process-ret-pred (:preds ret-such-that)))

               ;; Guardrails checks arg and ret specs separately, so in order
               ;; for the ret such-that predicates to be able to reference
               ;; argument bindings, we have to close over those. Spec has
               ;; separate :fn predicates for that and Malli doesn't do it at
               ;; all.
               closurify-ret-pred
               (fn [fn-pred]
                 (let [[pred-head [[orig-pred-params] & pred-bodies]]
                       (split-with (complement vector?) fn-pred)

                       pred-params (-> orig-pred-params
                                     set/map-invert
                                     (assoc :args clean-arg-vec)
                                     set/map-invert)
                       ret-pred    `(~@pred-head [~pred-params]
                                      ~@pred-bodies)]
                   `(~@pred-head [ret#]
                      (let [pred-args# {:args ~arg-syms :ret ret#}
                            ret-pred#  ~ret-pred]
                        (ret-pred# pred-args#)))))

               closurified-ret-preds
               (when processed-ret-preds
                 (cond->> (map closurify-ret-pred processed-ret-preds)
                   malli? (map #(do `[:fn ~%]))))

               final-fspec
               (if malli?
                 (if (or external-consumption? anon-fspec?)
                   (vec (concat
                          (when-not multi-arity-args? [:function])
                          [[:=> processed-arg-spec (extract-spec retspec)]]))
                   (let [ret (extract-spec retspec)]
                     {:args processed-arg-spec
                      :ret  (if closurified-ret-preds
                              `[:and ~ret ~@closurified-ret-preds]
                              ret)}))
                 (if (or external-consumption? anon-fspec?)
                   (concat
                     (when anon-fspec? [`s/fspec])
                     [:args processed-arg-spec]
                     [:ret (extract-spec retspec)]
                     (when processed-ret-preds
                       [:fn (if (next processed-ret-preds)
                              (cons `s/and processed-ret-preds)
                              (first processed-ret-preds))])
                     (when gen-fn [:gen gen-fn]))
                   (let [ret (extract-spec retspec)]
                     {:args processed-arg-spec
                      :ret  (if closurified-ret-preds
                              `(s/and ~ret ~@closurified-ret-preds)
                              ret)})))]
           (if nilable?
             (if malli?
               `[:maybe ~final-fspec]
               `(s/nilable ~final-fspec))
             final-fspec))))))

#?(:clj
   (let [unscrew-vec-unform
         (fn [unformed-arg]
           ;; Half-arsed workaround for spec bugs CLJ-2003 and CLJ-2021.
           (if-not (sequential? unformed-arg)
             unformed-arg
             (let [malformed-seq-destructuring? (every-pred seq? (comp #{:as '&} first))
                   [unformed malformed] (split-with (complement malformed-seq-destructuring?) unformed-arg)]
               (vec (concat unformed (apply concat malformed))))))

         ;; We need to have unique names for all arguments for Guardrails
         ;; checking and such-that predicates to work correctly, but with
         ;; anonymous vector or map destructuring of arguments (without an `:as`
         ;; attribute) we might not.
         process-arg
         (fn [index [arg-type arg]]
           (let [arg-prefix (format "arg%s_" (str (if (int? index)
                                                    (inc (long index))
                                                    index)))]
             (as-> arg arg
               (case arg-type
                 :sym [arg-type arg]
                 :seq [arg-type (update arg :as #(or % {:as :as :sym (gensym arg-prefix)}))]
                 :map [arg-type (update arg :as #(or % (gensym arg-prefix)))]))))]
     (defn- process-args [{:keys [args] :as fn-tail}]
       (let [conformed-reg-args (vec (->> args :args (map-indexed process-arg)))
             arg->sym           #(let [f (into {} [%])]
                                   (or
                                     (:sym f)
                                     (some-> f :seq :as :sym)
                                     (some-> f :map :as)))
             reg-arg-names      (mapv arg->sym conformed-reg-args)
             conformed-var-arg  (some->> args :varargs :form (process-arg "v"))
             arg-syms           (if conformed-var-arg
                                  (conj reg-arg-names (arg->sym conformed-var-arg))
                                  reg-arg-names)
             unform-arg         #(->> % (s/unform ::binding-form) unscrew-vec-unform)
             clean-arg-vec      (vec (concat
                                       (map unform-arg conformed-reg-args)
                                       (when conformed-var-arg [(unform-arg conformed-var-arg)])))
             raw-arg-vec        (if-not conformed-var-arg
                                  clean-arg-vec
                                  (vec (concat
                                         (pop clean-arg-vec)
                                         ['&]
                                         [(peek clean-arg-vec)])))]
         {::conformed-reg-args conformed-reg-args
          ::conformed-var-arg  conformed-var-arg
          ::arg-syms           arg-syms
          ::raw-arg-vec        raw-arg-vec
          ;; Includes the variadic arg like raw but without the `&`
          ::clean-arg-vec      clean-arg-vec}))))

#?(:clj
   ;; TODO make sure we check whether the variadic bodies are legit
   ;; Can not have more than one
   ;; Can not have one with more regular args than the variadic one
   ;; To what extent does the compiler already check this?
   (let [get-fspecs    (fn [malli? fn-tail]
                         (let [[param-count ^long variadic] (-> fn-tail :args count-args)
                               gspec (or (:gspec fn-tail)
                                       (s/conform ::gspec
                                         (vec (concat (repeat param-count 'any?)
                                                (when (> variadic 0)
                                                  (if malli?
                                                    `[[:* any?]]
                                                    `[(s/* any?)]))
                                                '[=> any?]))))]
                           [(->> (if (> variadic 0) "n" param-count)
                              (str "arity-")
                              keyword)
                            (gspec->fspec*
                              (process-args fn-tail)
                              gspec
                              {::multi-arity-args?     true
                               ::malli?                malli?
                               ::external-consumption? true})]))
         get-spec-part (fn [part spec]
                         (->> spec
                           (drop-while (complement #{part}))
                           second))]
     (defn- generate-external-fspec
       "Generates function specs or schemas for external consumption by native
       clojure.spec or Malli tools. Data specs for internal Guardrails checking
       are different in that we have one argument and one return data spec for
       each function arity.

       Native function specs are in some ways more complex (they need to account
       for multi-arity in a single branching spec, clojure.spec has :fn
       predicates separate from the :ret predicates, etc.) and in other ways
       simpler (Malli doesn't have proper predicate support in function schemas,
       so we just drop those).

       In any case, they are different enough that they require separate
       handling, which is what we do here. You can macroexpand a `(>defn ...)`
       for details."
       [conformed-fn-tail-or-tails malli?]
       (case (key conformed-fn-tail-or-tails)
         :arity-1
         (when-let [gspec (-> conformed-fn-tail-or-tails val :gspec)]
           (gspec->fspec*
             (process-args (val conformed-fn-tail-or-tails))
             gspec
             {::malli?                malli?
              ::external-consumption? true}))
         :arity-n
         (when (some :gspec (val conformed-fn-tail-or-tails))
           (if malli?
             (let [fspecs (map (partial get-fspecs true) (val conformed-fn-tail-or-tails))]
               `[:function ~@(mapcat second fspecs)])
             (let [fspecs           (map (partial get-fspecs false) (val conformed-fn-tail-or-tails))
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
                          multi-ret-clause)])))))))))

#?(:clj
   (defn generate-fdef
     [env forms]
     (let [{[type fn-name] :name bs :bs} (s/conform ::>fdef-args forms)]
       (if (:guardrails/malli? env)
         (when (= type :sym)
           `(malli.core/=> ~fn-name [~@(generate-external-fspec bs true)]))
         (case type
           :sym `(s/fdef ~fn-name ~@(generate-external-fspec bs false))
           :key `(s/def ~fn-name (s/fspec ~@(generate-external-fspec bs false))))))))

;; The now-nano was taken from taoensso/encore. I didn't want to include that entire dep just for this one
;; function. The are covered by Eclipse Public License - v 1.0. See https://github.com/taoensso/encore
#?(:cljs (def ^:no-doc js-?window (when (exists? js/window) js/window))) ; Present iff in browser

#?(:cljs
   (defn oget "Like `get` for JS objects."
     ([k] (gobj/get js-?window (name k)))
     ([o k] (gobj/get o (name k) nil))
     ([o k not-found] (gobj/get o (name k) not-found))))

#?(:clj
   (defn now-nano
     "Returns current value of best-resolution time source as nanoseconds."
     {:inline (fn [] `(System/nanoTime))}
     ^long [] (System/nanoTime))

   :cljs
   (def now-nano
     "Returns current value of best-resolution time source as nanoseconds."
     (let [perf (oget js-?window "performance")
           pf   (when perf
                  (or
                    (oget perf "now") (oget perf "mozNow") (oget perf "webkitNow")
                    (oget perf "msNow") (oget perf "oNow")))]
       (if (and perf pf)
         (fn [] (Math/floor (* 1e6 (.call pf perf))))
         (fn [] (* 1e6 (js/Date.now)))))))

#?(:clj
   (defn- throttle-form [add-throttle?]
     (if add-throttle?
       `(let [ltime#     (long (deref ~'__gr_vstart-time))
              now-ns#    (now-nano)
              elapsed#   (- now-ns# ltime#)
              throttle?# (> (/ (double (deref ~'__gr_vncalls)) elapsed#) ~'__gr_max-calls-per-ns)]
          (when-not throttle?#
            (vswap! ~'__gr_vncalls (comp inc long)))
          (when (> elapsed# ~'__gr_reset-stats-ns)
            (vreset! ~'__gr_vstart-time now-ns#)
            (vreset! ~'__gr_vncalls 0))
          throttle?#)
       false)))

#?(:clj
   (defn- process-defn-tail
     "This does the top-level Guardrails procesing of function tails (args,
     gspec, body) entirely unconcerned with function arity."
     [cfg fn-tail]
     (let [{:keys                                                      [env fn-name]
            {max-checks-per-second :guardrails/mcps
             :guardrails/keys      [use-stderr? compact? trace? stack-trace]
             :keys                 [throw? tap>? disable-exclusions?]} :config} cfg
           {:guardrails/keys [validate-fn explain-fn humanize-fn malli?]} env
           cljs?           (cljs-env? env)

           {:com.fulcrologic.guardrails.core/keys
            [conformed-var-arg arg-syms raw-arg-vec]
            :as processed-params}
           (process-args fn-tail)

           {:keys [body args gspec]} fn-tail
           fspec           (when gspec
                             (gspec->fspec* processed-params gspec {::malli? malli?}))
           [prepost orig-body-forms] (case (key body)
                                       :prepost+body [(-> body val :prepost)
                                                      (-> body val :body)]
                                       :body [nil (val body)])
           {:keys [file line]} (if cljs?
                                 (meta fn-name)
                                 {:file #?(:clj *file* :cljs "N/A")
                                  :line (some-> env :form meta :line)})
           body-forms      orig-body-forms
           where           (str file ":" line " " fn-name "'s")
           argspec         (gensym "argspec")
           nspc            (if cljs? (-> env :ns :name str) (name (ns-name *ns*)))
           opts            {:fn-name                where
                            :guardrails/fqnm        (str (str nspc) "/" (str fn-name))
                            :guardrails/trace?      trace?
                            :guardrails/compact?    compact?
                            :guardrails/stack-trace stack-trace
                            :guardrails/use-stderr? use-stderr?
                            :guardrails/malli?      malli?
                            :tap>?                  tap>?
                            :throw?                 throw?
                            :vararg?                (boolean conformed-var-arg)
                            :humanize-opts          (if malli?
                                                      (get (gr.cfg/get-env-config) :malli {})
                                                      (get (gr.cfg/get-env-config) :expound {}))
                            :guardrails/validate-fn validate-fn
                            :guardrails/explain-fn  explain-fn
                            :guardrails/humanize-fn humanize-fn}
           exclusion-coord (if disable-exclusions?
                             [:undefined/function :undefined]
                             [(keyword nspc (name fn-name)) (keyword nspc)])
           args-check      `(when ~argspec
                              (when-not (gr.cfg/-excluded? ~(first exclusion-coord) ~(second exclusion-coord))
                                (run-check ~(assoc opts :args? true) ~argspec ~arg-syms)))
           retspec         (gensym "retspec")
           ret             (gensym "ret")
           add-throttling? (number? max-checks-per-second)
           ret-check       `(when (and ~retspec
                                    (not (gr.cfg/-excluded? ~(first exclusion-coord) ~(second exclusion-coord))))
                              (run-check ~(assoc opts :args? false) ~retspec ~ret))
           real-function   `(fn ~'guardrails-wrapper ~raw-arg-vec ~@body-forms)
           f               (gensym "f")
           call            (if (boolean conformed-var-arg)
                             `(cond
                                (map? ~(last arg-syms)) (apply ~f ~@(butlast arg-syms) (apply concat (last ~arg-syms)))
                                (seq ~(last arg-syms)) (apply ~f ~@arg-syms)
                                :else (~f ~@(butlast arg-syms)))
                             `(~f ~@arg-syms))
           throttle-form   (throttle-form add-throttling?)
           bnd             (if cljs? `with-redefs 'clojure.core/binding)]
       `(~@(remove nil? [raw-arg-vec prepost])
          (~bnd [utils/*backtrace* (or utils/*backtrace* (utils/new-backtrace 10))]
            (utils/backtrace-enter ~(str nspc) ~(str fn-name) ~@arg-syms)
            (try
              (let [{~argspec :args ~retspec :ret} ~fspec
                    throttle# ~throttle-form]
                (when-not throttle#
                  ~args-check)
                (let [~f ~real-function
                      ~ret ~call]
                  (when-not throttle#
                    ~ret-check)
                  ~ret))
              (finally
                (utils/backtrace-exit))))))))

#?(:clj
   (defn- generate-defn
     [forms private env]
     (let [conformed-gdefn    (s/conform ::>defn-args forms)
           conformed-fn-tails (:bs conformed-gdefn)
           arity              (key conformed-fn-tails)
           fn-name            (:name conformed-gdefn)
           docstring          (:docstring conformed-gdefn)
           {:guardrails/keys [malli?]} env
           raw-meta-map       (merge
                                (:meta conformed-gdefn)
                                {::guardrails true})
           ;;; Assemble the config
           {max-checks-per-second :guardrails/mcps
            :keys                 [defn-macro] :as config}
           (gr.cfg/merge-config env (meta fn-name) raw-meta-map)

           add-throttling?    (number? max-checks-per-second)
           defn-sym           (cond
                                defn-macro (with-meta (symbol defn-macro) {:private private})
                                private 'defn-
                                :else 'defn)
           ;;; Code generation
           external-fdef-body (generate-external-fspec conformed-fn-tails malli?)
           fdef               (when external-fdef-body
                                (if malli?
                                  ;; REVIEW: Since we're already adding the
                                  ;; Malli function schema to the metadata
                                  ;; below, there's probably no need to define
                                  ;; it here as well.
                                  nil #_`(malli.core/=> ~fn-name [~@external-fdef-body])
                                  `(s/fdef ~fn-name ~@external-fdef-body)))
           meta-map           (merge
                                raw-meta-map
                                (when malli? {:malli/schema `[~@external-fdef-body]}))
           processed-fn-tails (let [process-cfg      {:env     env
                                                      :config  config
                                                      :fn-name fn-name}
                                    fn-tail-or-tails (val conformed-fn-tails)]
                                (case arity
                                  :arity-1 (process-defn-tail process-cfg fn-tail-or-tails)
                                  :arity-n (map (partial process-defn-tail process-cfg) fn-tail-or-tails)))
           main-defn          `(~@(remove nil? [defn-sym fn-name docstring meta-map])
                                 ~@processed-fn-tails)
           throttle-decls     (if add-throttling?
                                `[~'__gr_vncalls (volatile! 0)
                                  ~'__gr_vstart-time (volatile! 0)
                                  ~'__gr_reset-stats-ns 5e9
                                  ~'__gr_max-calls-per-ns (/ ~max-checks-per-second 9.0e8) ; fudge factor for nano timing inaccuracies
                                  ~'__gr_actual-calls (volatile! 0)]
                                [])]
       `(let ~throttle-decls
          ~@(remove nil? [fdef main-defn])))))

#?(:clj
   ;;;; Main macros and public API
   (s/def ::>defn-args
     (s/and seq?                                            ; REVIEW
       (s/cat :name simple-symbol?
         :docstring (s/? string?)
         :meta (s/? map?)
         :bs (s/alt :arity-1 ::args+gspec+body
               ;; TODO: add tail-attr-map support after this
               :arity-n (s/+ (s/and seq? ::args+gspec+body)))))))

#?(:clj
   (defn >defn* [env form body {:keys [private? guardrails/malli?] :as opts}]
     (let [cfg  (gr.cfg/get-env-config)
           mode (gr.cfg/mode cfg)]
       (cond
         (not cfg) (clean-defn 'defn body)
         (= :pro mode) `(do
                          (defn ~@body)
                          ~(gr.pro/>defn-impl env body opts))
         (#{:runtime :all} mode)
         (cond-> (remove nil? (generate-defn body private? (assoc env :form form :guardrails/malli? malli?)))
           (cljs-env? env) clj->cljs
           (= :all mode) (-> vec (conj (gr.pro/>defn-impl env body opts)) seq))))))

#?(:clj
   (defmacro >defn
     "Like defn, but requires a (nilable) gspec definition and generates
     additional `s/fdef` function spec definition and validation code."
     {:arglists '([name doc-string? attr-map? [params*] gspec prepost-map? body?]
                  [name doc-string? attr-map? ([params*] gspec prepost-map? body?) + attr-map?])}
     [& forms]
     (let [env (merge &env `{:guardrails/validate-fn s/valid?
                             :guardrails/explain-fn  s/explain-data
                             :guardrails/humanize-fn humanize-spec})]
       (>defn* env &form forms {:private? false}))))

#?(:clj
   (s/fdef >defn :args ::>defn-args))

#?(:clj
   (defmacro >defn-
     "Like defn-, but requires a (nilable) gspec definition and generates
     additional `s/fdef` function spec definition and validation code."
     {:arglists '([name doc-string? attr-map? [params*] gspec prepost-map? body?]
                  [name doc-string? attr-map? ([params*] gspec prepost-map? body?) + attr-map?])}
     [& forms]
     (let [env (merge &env `{:guardrails/validate-fn s/valid?
                             :guardrails/explain-fn  s/explain-data
                             :guardrails/humanize-fn humanize-spec})]
       (>defn* env &form forms {:private? true}))))

(comment
  (>defn- test-function [] [=> nil?] nil)
  (clojure.pprint/pprint (meta #'test-function))
  (assert (true? (:private (meta #'test-function))))

  (>defn test-function2 [] [=> nil?] nil)
  (assert (nil? (:private (meta #'test-function2)))))

#?(:clj
   (s/fdef >defn- :args ::>defn-args))

#?(:clj
   (defmacro >def
     "Just like Clojure s/def, except there is a stub for this in the `noop`
     namespace, which you can substitute via CLJS build parameters, turning it
     into code that can be dead-code eliminated in a CLJS production build. See
     the docstring for the `com.fulcrologic.guardrails.noop` namespace."
     ([k spec-form]
      (cond-> `(s/def ~k ~spec-form)
        (cljs-env? &env) clj->cljs))
     ([k _doc spec-form]
      `(>def ~k ~spec-form))))

#?(:clj
   (s/def ::>fdef-args
     (s/and seq?                                            ;REVIEW
       (s/cat :name (s/or :sym symbol? :key qualified-keyword?)
         :bs (s/alt :arity-1 ::args+gspec+body
               :arity-n (s/+ (s/and seq? ::args+gspec+body)))))))

#?(:clj
   (defmacro >fdef
     "Defines an fspec using gspec syntax – pretty much a `>defn` without the body.

     `name` can be a symbol or a qualified keyword, depending on whether the
     fspec is meant to be registered as a top-level fspec (=> s/fdef fn-sym ..)
     or used in other specs (=> s/def ::spec-keyword (s/fspec ...)). "
     {:arglists '([name [params*] gspec]
                  [name ([params*] gspec) +])}
     [& forms]
     (when-let [cfg (gr.cfg/get-env-config)]
       `(do
          ~(when (#{:pro :all} (gr.cfg/mode cfg))
             (gr.pro/>fdef-impl &env forms))
          ~(cond-> (remove nil? (generate-fdef &env forms))
             (cljs-env? &env) clj->cljs)))))

#?(:clj
   (s/fdef >fdef :args ::>fdef-args))

#?(:clj
   ;; TODO: clean >fn (no gspec)
   (defmacro >fn [& forms] `(fn ~@forms)))

#?(:clj
   (defmacro >fspec [& forms]
     (gr.pro/>fspec-impl &env forms)))
