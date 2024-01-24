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
               [com.fulcrologic.guardrails.utils :refer [cljs-env? clj->cljs]]]
        :cljs [[com.fulcrologic.guardrails.impl.externs]])
    [com.fulcrologic.guardrails.utils :refer [strip-colors]]
    [com.fulcrologic.guardrails.config :as gr.cfg]
    [clojure.core.async :as async]
    [clojure.spec.alpha :as s]
    [clojure.string :as string]
    [com.fulcrologic.guardrails.utils :as utils]
    [taoensso.encore :as enc]
    [taoensso.tufte :refer [profile p]]
    [expound.alpha :as exp]))

;; It doesn't actually matter what these are bound to, they are stripped by
;; the macros they're used in and never end up in the final code. This is just
;; so they can be used without '=> cannot be resolved' errors in the IDE.
(def => :ret)
(def | :st)
(def <- :gen)

(def ^:private global-context (atom (list)))

(defn enter-global-context!
  "Push a global context, accessible from all threads, onto a stack. Used to add
  information to what guardrails will report when a function failed a check."
  [ctx]
  (swap! global-context (partial cons ctx)))

(defn leave-global-context!
  "Pops a global context (see `enter-global-context!`). Should be passed the
  same context that was pushed, although is not enforced, as it's only to be
  easily compatible with fulcro-spec's hooks API."
  [ctx]
  (swap! global-context rest))

#?(:clj
   (defmacro with-global-context
     "Wraps the body with an enter and leave global context.
      Will always call leave as it uses a try finally block.
      See `enter-global-context!`."
     [ctx & body]
     `(do (enter-global-context! ~ctx)
          (try ~@body
               (finally
                 (leave-global-context! ~ctx))))))

(defn- get-global-context [] (first @global-context))


(defonce pending-check-channel (async/chan (async/dropping-buffer 10000)))

(defonce async-go-channel
  (async/go-loop [check (async/<! pending-check-channel)]
    (if check
      (do
        (try
          (check)
          (catch #?(:clj Exception :cljs :default) _))
        (recur (async/<! pending-check-channel)))
      (println "Guardrails ASYNC LOOP STOPPED ****************************************"))))

;; runtime checking (both clj and cljs
(defn- output-fn [data]
  (let [{:keys [level ?err msg_ ?ns-str ?file hostname_
                timestamp_ ?line]} data]
    (str
      (string/upper-case (name level)) " "
      (force msg_)
      (when-let [err ?err]
        (str "\n" (utils/stacktrace err))))))

(defn now-ms [] #?(:clj  (System/currentTimeMillis)
                   :cljs (inst-ms (js/Date.))))

(def tap (resolve 'tap>))

(defn humanize-spec [explain-data {:guardrails/keys [expound-options]}]
  (with-out-str
    ((exp/custom-printer expound-options) explain-data)))

(defn run-check [{:guardrails/keys [malli? validate-fn explain-fn humanize-fn]
                  :keys            [tap>? args? vararg? expound-opts callsite throw? fn-name]}
                 spec
                 value]
  (p `run-check
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
        (when-not (p `actual-validation (validate-fn spec specable-args))
          (let [explain-data  (explain-fn spec specable-args)
                explain-human (humanize-fn explain-data {:guardrails/expound-options expound-opts})
                description   (str
                                "\n"
                                fn-name
                                (if args? " argument list" " return type") "\n"
                                explain-human)
                context       (get-global-context)]
            (when (and tap tap>?)
              (tap #:com.fulcrologic.guardrails
                      {:_/type        :com.fulcrologic.guardrails/validation-error
                       :fn-name       fn-name
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
              (utils/report-problem (str description "\n" (utils/stacktrace (or callsite (ex-info "" {}))))))))
        (catch #?(:cljs :default :clj Throwable) e
          (utils/report-exception e (str "BUG: Internal error in " (if malli? "Malli.\n" "expound or clojure spec.\n"))))
        (finally
          (let [duration (- (now-ms) start)]
            (when (> duration 100)
              (utils/report-problem (str "WARNING: " fn-name " " (if args? "argument specs" "return spec") " took " duration "ms to run."))))))
      (when @valid-exception
        (throw @valid-exception))))
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
                            :attr (s/? map?))))))))

;;;; Main code generating functions

#?(:clj
   (defn- gspec->fspec*
     [{:com.fulcrologic.guardrails.core/keys
       [conformed-reg-args conformed-var-arg arg-syms clean-arg-vec]
       :as processed-args}
      conformed-gspec
      {:com.fulcrologic.guardrails.core/keys
       [anon-fspec? multi-arity-args? nilable? malli? external-consumption?]
       :as opts}]
     (let [{argspec-def              :args
            retspec                  :ret
            fn-such-that             :fn-such-that
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

               arg-binding-destruct
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

               process-arg-pred
               (fn process-arg-pred [{:keys [name args body]}]
                 (let [bindings (if-let [anon-arg (some-> args :args first second)]
                                  (if malli?
                                    (into arg-binding-destruct [:as anon-arg])
                                    (assoc arg-binding-destruct :as anon-arg))
                                  arg-binding-destruct)
                       function (remove nil? `(fn ~name [~bindings] ~body))]
                   (if malli?
                     [:fn function]
                     function)))

               processed-args
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
                                                (fn [index _]
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

               process-ret-pred
               (fn process-ret-pred [{:keys [name args body]}]
                 (let [anon-arg       (some-> args :args first second)
                       ret-sym        (gensym "ret__")
                       bindings       [{(if multi-arity-args?
                                          ['_ arg-binding-destruct]
                                          arg-binding-destruct) :args
                                        ret-sym                 :ret}]
                       processed-body (if anon-arg
                                        (walk/postwalk-replace {anon-arg ret-sym} body)
                                        body)]
                   (remove nil? `(fn ~name ~bindings ~processed-body))))

               processed-fn-preds
               (when fn-such-that
                 (map process-ret-pred (:preds fn-such-that)))

               retify-fn-pred
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

               ret+fn-preds
               (when processed-fn-preds
                 (cond->> (map retify-fn-pred processed-fn-preds)
                   malli? (map #(do `[:fn ~%]))))

               final-fspec
               (if malli?
                 (if (or external-consumption? anon-fspec?)
                   (vec (concat
                          [:function]
                          [[:=> processed-args (extract-spec retspec)]]))
                   (let [ret (extract-spec retspec)]
                     {:args processed-args
                      :ret  (if ret+fn-preds
                              `[:and ~ret ~@ret+fn-preds]
                              ret)}))
                 (if (or external-consumption? anon-fspec?)
                   (concat
                     (when anon-fspec? [`s/fspec])
                     [:args processed-args]
                     [:ret (extract-spec retspec)]
                     (when processed-fn-preds
                       [:fn (if (next processed-fn-preds)
                              (cons `s/and processed-fn-preds)
                              (first processed-fn-preds))])
                     (when gen-fn [:gen gen-fn]))
                   (let [ret (extract-spec retspec)]
                     {:args processed-args
                      :ret  (if ret+fn-preds
                              `(s/and ~ret ~@ret+fn-preds)
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

         process-arg
         (fn [index [arg-type arg]]
           (let [arg-prefix (format "arg%s_" (str (if (int? index)
                                                    (inc index)
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
                         (let [[param-count variadic] (-> fn-tail :args count-args)
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
               (mapcat second fspecs))
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
        'fn?          "Function"})))

#?(:clj
   (declare get-gspec-type))

#?(:clj
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
                       (#{'and} op) [(-> spec-def second)]  ; TODO
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
           "*")))))

#?(:clj
   (defn- get-gspec-type [conformed-gspec]
     (let [argspec-def (:args conformed-gspec)
           args-jstype (if-not argspec-def
                         ""
                         (->> (-> conformed-gspec :args :args)
                           (map (partial get-type false))
                           (string/join ", ")))
           ret-jstype  (get-type false (:ret conformed-gspec))]
       (str "function(" args-jstype "): " ret-jstype))))

#?(:clj
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
                          {:jsdoc [(str "@return {" (string/join "|" ret-types) "}")]})))))

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

(defn callsite-exception []
  #?(:cljs (js/Error. "")
     :clj  (AssertionError. "")))

#?(:clj
   (defn- throttle-form [add-throttle?]
     (if add-throttle?
       `(let [ltime#     (deref ~'__gr_vstart-time)
              now-ns#    (enc/now-nano*)
              elapsed#   (- now-ns# ltime#)
              throttle?# (> (/ (double (deref ~'__gr_vncalls)) elapsed#) ~'__gr_max-calls-per-ns)]
          (when-not throttle?#
            (vswap! ~'__gr_vncalls inc))
          (when (> elapsed# ~'__gr_reset-stats-ns)
            (vreset! ~'__gr_vstart-time now-ns#)
            (vreset! ~'__gr_vncalls 0))
          throttle?#)
       false)))

#?(:clj
   (defn- process-defn-body
     [cfg fn-tail]
     (let [{:keys                                                      [env fn-name]
            {max-checks-per-second :guardrails/mcps
             :keys                 [throw? tap>? disable-exclusions?]} :config} cfg
           {:guardrails/keys [validate-fn explain-fn humanize-fn malli?]
            :keys            [async-checks?]} env
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
           expound-opts    (get (gr.cfg/get-env-config) :expound {})
           opts            {:fn-name                where
                            :guardrails/malli?      malli?
                            :tap>?                  tap>?
                            :throw?                 throw?
                            :vararg?                (boolean conformed-var-arg)
                            :expound-opts           expound-opts
                            :guardrails/validate-fn validate-fn
                            :guardrails/explain-fn  explain-fn
                            :guardrails/humanize-fn humanize-fn}
           gosym           (if cljs? 'cljs.core.async/go 'clojure.core.async/go)
           putsym          (if cljs? 'cljs.core.async/>! 'clojure.core.async/>!)
           exclusion-coord (if disable-exclusions?
                             [:undefined/function :undefined]
                             [(keyword nspc (name fn-name)) (keyword nspc)])
           args-check      (if async-checks?
                             `(when-not (gr.cfg/-excluded? ~(first exclusion-coord) ~(second exclusion-coord))
                                (let [e# (callsite-exception)]
                                  (~gosym
                                    (~putsym pending-check-channel (fn []
                                                                     (when ~argspec
                                                                       (run-check (assoc
                                                                                    ~(assoc opts :args? true)
                                                                                    :callsite e#)
                                                                         ~argspec
                                                                         ~arg-syms)))))))
                             `(when ~argspec
                                (when-not (gr.cfg/-excluded? ~(first exclusion-coord) ~(second exclusion-coord))
                                  (run-check ~(assoc opts :args? true) ~argspec ~arg-syms))))
           retspec         (gensym "retspec")
           ret             (gensym "ret")
           add-throttling? (number? max-checks-per-second)
           ret-check       (if async-checks?
                             `(when-not (gr.cfg/-excluded? ~(first exclusion-coord) ~(second exclusion-coord))
                                (let [e# (callsite-exception)]
                                  (~gosym
                                    (~putsym pending-check-channel (fn [] (when ~retspec (run-check (assoc
                                                                                                      ~(assoc opts :args? false)
                                                                                                      :callsite e#)
                                                                                           ~retspec
                                                                                           ~ret)))))))
                             `(when (and ~retspec (not (gr.cfg/-excluded? ~(first exclusion-coord) ~(second exclusion-coord))))
                                (run-check ~(assoc opts :args? false) ~retspec ~ret)))
           real-function   `(fn ~raw-arg-vec ~@body-forms)
           f               (gensym "f")
           call            (if (boolean conformed-var-arg)
                             `(cond
                                (map? ~(last arg-syms)) (apply ~f ~@(butlast arg-syms) (apply concat (last ~arg-syms)))
                                (seq ~(last arg-syms)) (apply ~f ~@arg-syms)
                                :else (~f ~@(butlast arg-syms)))
                             `(~f ~@arg-syms))
           throttle-form   (throttle-form add-throttling?)]
       `(~@(remove nil? [raw-arg-vec prepost])
          (let [{~argspec :args ~retspec :ret} ~fspec
                throttle# ~throttle-form]
            (when-not throttle#
              ~args-check)
            (let [~f ~real-function
                  ~ret ~call]
              (when-not throttle#
                ~ret-check)
              ~ret))))))

#?(:clj
   (defn- generate-defn
     [forms private env]
     (let [conformed-gdefn     (s/conform ::>defn-args forms)
           conformed-fn-tails  (:bs conformed-gdefn)
           arity               (key conformed-fn-tails)
           fn-name             (:name conformed-gdefn)
           docstring           (:docstring conformed-gdefn)
           malli?              (:guardrails/malli? env)
           raw-meta-map        (merge
                                 (:meta conformed-gdefn)
                                 {::guardrails true})
           ;;; Assemble the config
           {max-checks-per-second :guardrails/mcps
            :keys                 [defn-macro] :as config} (gr.cfg/merge-config env (meta fn-name) raw-meta-map)
           add-throttling?     (number? max-checks-per-second)
           defn-sym            (cond defn-macro (with-meta (symbol defn-macro) {:private private})
                                     private 'defn-
                                     :else 'defn)
           ;;; Code generation
           external-fdef-body  (generate-external-fspec conformed-fn-tails malli?)
           fdef                (when external-fdef-body
                                 (if malli?
                                   ;; REVIEW: Since we're already adding the
                                   ;; Malli function schema to the metadata
                                   ;; below, there's probably no need to define
                                   ;; it here as well.
                                   nil #_`(malli.core/=> ~fn-name [~@external-fdef-body])
                                   `(s/fdef ~fn-name ~@external-fdef-body)))
           meta-map            (merge
                                 raw-meta-map
                                 (when malli? {:malli/schema `[~@external-fdef-body]})
                                 (when-not malli? (generate-type-annotations env conformed-fn-tails)))
           processed-fn-bodies (let [process-cfg      {:env     env
                                                       :config  config
                                                       :fn-name fn-name}
                                     fn-tail-or-tails (val conformed-fn-tails)]
                                 (case arity
                                   :arity-1 (process-defn-body process-cfg fn-tail-or-tails)
                                   :arity-n (map (partial process-defn-body process-cfg) fn-tail-or-tails)))
           main-defn           `(~@(remove nil? [defn-sym fn-name docstring meta-map])
                                  ~@processed-fn-bodies)
           throttle-decls      (if add-throttling?
                                 `[~'__gr_vncalls (volatile! 0)
                                   ~'__gr_vstart-time (volatile! 0)
                                   ~'__gr_reset-stats-ns 5e9
                                   ~'__gr_max-calls-per-ns (/ ~max-checks-per-second 9.0e8) ; fudge factor for nano timing inaccuracies
                                   ~'__gr_actual-calls (volatile! 0)]
                                 [])]
       `(let ~throttle-decls
          ~@(remove nil? [fdef `(declare ~fn-name) main-defn])))))

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
     (let [cfg    (gr.cfg/get-env-config)
           mode   (gr.cfg/mode cfg)
           async? (gr.cfg/async? cfg)]
       (cond
         (not cfg) (clean-defn 'defn body)
         (#{:copilot :pro} mode) `(do (defn ~@body)
                                      ~(gr.pro/>defn-impl env body opts))
         (#{:runtime :all} mode)
         (cond-> (remove nil? (generate-defn body private? (assoc env :form form :async-checks? async? :guardrails/malli? malli?)))
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
  (assert (nil? (:private (meta #'test-function2)))),)

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
          ~(when (#{:pro :copilot :all} (gr.cfg/mode cfg))
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
