(ns com.fulcrologic.guardrails.impl.parser
  (:require
    [clojure.set :as set]
    [clojure.walk :as walk]
    [com.fulcrologic.guardrails.registry :as gr.reg]
    [com.fulcrologic.guardrails.utils :as utils]))

(defn init-parser-state
  ([args] (init-parser-state args {}))
  ([args opts] (init-parser-state {} args opts))
  ([result args opts]
   {::result       result
    ::args         args
    ::opts         opts
    ::initial-args args}))

(defn next-args
  ([state] (next-args state next))
  ([state next-fn]
   (-> state
     (update ::args next-fn))))

(defn update-result [state f & args]
  (update state ::result (partial apply f) args))

(defn parser-error [state fmt & args]
  (ex-info (apply format (str "[guardrails/parser]: " fmt) args) state))

(defn loop-over-args
  [state done? & {:keys [result-fn state-fn]}]
  (assert (not (and result-fn state-fn))
    "Cannot use both result-fn and state-fn")
  (let [action (fn [state arg]
                 (if result-fn
                   (update-result state result-fn arg)
                   (state-fn state arg)))]
    (loop [{:as state, [arg] ::args} state]
      (cond
        (done? arg) state
        (not arg) (throw (parser-error state "syntax error: expected %s" done?))
        :else (recur (-> state (action arg) next-args))))))

;; DEFN PARSERS

(def gen? #{:gen '<-})
(def ret? #{:ret '=>})
(def such-that? #{:st '|})

(def append (fnil conj []))

(defn sym-meta
  [{:as state [sym] ::args}]
  (assoc state ::fn-meta (meta sym)))

(defn function-name
  [{:as state, [?name] ::args}]
  (if (simple-symbol? ?name)
    (-> state
      (sym-meta)
      (update-result assoc ::gr.reg/fn-name `(quote ~?name))
      (update-result assoc ::gr.reg/fn-ref ?name)
      (next-args))
    (throw (parser-error state "Invalid function name: <%s>. Was not a simple symbol." ?name))))

(defn var-name
  [{:as state, [?name] ::args}]
  (if (qualified-symbol? ?name)
    (-> state
      (sym-meta)
      (update-result assoc ::gr.reg/var-name `(quote ~?name))
      (update-result assoc ::gr.reg/fn-ref ?name)
      (next-args))
    (throw (ex-info (format "%s is not fully qualified symbol" ?name) {}))))

(defn lambda-name [{:as state, [?name] ::args}]
  (cond-> state
    (symbol? ?name)
    (-> (update-result assoc ::gr.reg/lambda-name `(quote ~?name))
      (next-args))))

(defn optional-docstring
  [{:as state, [candidate] ::args}]
  (cond-> state
    (string? candidate)
    (next-args)))

(defn gspec-metadata
  [{:as state, gspec ::args}]
  (let [metadata (some-> (merge (::fn-meta state) (meta gspec))
                   (dissoc :source :file :line :end-line :column :end-column))]
    (cond-> state
      (seq metadata) (update-result assoc ::gr.reg/metadata metadata))))

(defn resolve-spec [externs quoted-spec]
  (walk/postwalk
    #(or (when (symbol? %)
           (some-> externs
             (get-in [`(quote ~%) ::gr.reg/extern-name])
             second))
       %)
    quoted-spec))

(defn argument-specs
  [{:as state, ::keys [args], {:keys [externs]} ::opts}]
  (loop-over-args state
    (set/union ret? such-that?)
    :state-fn
    (fn [state arg-spec]
      (let [fq-arg-spec (resolve-spec externs arg-spec)]
        (-> state
          (assoc-in [::gr.reg/spec-registry `(quote ~fq-arg-spec)] arg-spec)
          (update-result update ::gr.reg/argument-types append (pr-str arg-spec))
          (update-result update ::gr.reg/quoted.argument-specs append `(quote ~fq-arg-spec)))))))

(defn replace-arglist [lambda-form new-arglist]
  (apply list (first lambda-form) new-arglist (rest (rest lambda-form))))

(defn argument-predicates
  [{:as state, [lookahead & remainder] ::args} arglist]
  (if (such-that? lookahead)
    (-> state
      (next-args)
      (loop-over-args ret? :result-fn
        (fn [result arg]
          (update result ::gr.reg/argument-predicates
            append (replace-arglist arg arglist)))))
    state))

(defn return-type
  [{:as state, [lookahead return-spec] ::args, {:keys [externs]} ::opts}]
  (if (ret? lookahead)
    (let [fq-return-spec (resolve-spec externs return-spec)]
      (-> state
        (assoc-in [::gr.reg/spec-registry `(quote ~fq-return-spec)] return-spec)
        (update-result assoc ::gr.reg/return-type (pr-str return-spec))
        (update-result assoc ::gr.reg/quoted.return-spec `(quote ~fq-return-spec))
        (next-args nnext)))
    (throw (parser-error state "Syntax error: expected a return type!"))))

(defn such-that
  [{:as state, ::keys [result], [lookahead & remainder :as args] ::args}]
  (if (such-that? lookahead)
    (-> state
      (next-args)
      (loop-over-args (some-fn not gen?) :result-fn
        (fn [result arg]
          ;; TODO: return predicate cannot resolve from arglist
          (update result ::gr.reg/return-predicates append arg))))
    state))

(defn generator
  [{:as state, [lookahead & remainder] ::args}]
  (cond-> state (gen? lookahead)
    (->
      (next-args nnext)
      (update-result assoc ::gr.reg/generator (first remainder)))))

(defn gspec-parser
  [{:as state, gspec ::args} arglist]
  (-> state
    (gspec-metadata)
    (argument-specs)
    (argument-predicates arglist)
    (return-type)
    (such-that)
    (generator)))

(defn >fn? [x]
  (and (seq? x) (symbol? (first x))
    (= ">fn" (name (first x)))))

(defn lambda:env->fn:impl [binds fn-form]
  (let [env (gensym "env$")]
    `(fn [~env]
       (let [~@(mapcat
                 (fn [sym] [sym `(com.fulcrologic.copilot.artifacts/lookup-symbol ~env '~sym)])
                 binds)]
         (fn ~@(rest fn-form))))))

(defmacro lambda:env->fn [& args]
  (apply lambda:env->fn:impl args))

(defn select-simple-symbols [body]
  (let [syms (atom #{})]
    (->> body (walk/postwalk #(do (when (simple-symbol? %) (swap! syms conj %)) %)))
    @syms))

(defn binds-for-lambda [{::gr.reg/keys [lambda-name]} lambda-body extern-symbols]
  (-> (next lambda-body)                                    ;; drop >fn symbol itself
    (cond-> lambda-name next)
    (->> (drop 2))                                          ;; drop arglist & gspec
    (select-simple-symbols)
    (set/difference (set extern-symbols))))

(declare parse-fn)

(defn location-of-lambda [lambda-form]
  ((juxt :line :column) (meta lambda-form)))

(defn parse-lambdas [body extern-symbols]
  (into {}
    (mapcat
      (fn [lambda-form]
        (let [function (parse-fn (rest lambda-form) extern-symbols)
              binds    (binds-for-lambda function lambda-form extern-symbols)
              env->fn  (lambda:env->fn:impl binds lambda-form)]
          (conj (seq (::gr.reg/lambdas function))
            (vector (location-of-lambda lambda-form)
              (-> function
                (dissoc ::gr.reg/lambdas)
                (assoc ::gr.reg/env->fn env->fn)
                ;; TODO: only for debugging
                (assoc :DBG/env->fn `(quote ~env->fn))))))))
    (let [fns (atom [])]
      (->> body (walk/prewalk #(do (when (>fn? %) (swap! fns conj %)) %)))
      @fns)))

(defn arity-body? [b] (seq? b))

(defn body-arity [arglist]
  (if (contains? (set arglist) '&)
    :n (count arglist)))

(defn single-arity
  [{:as state, [arglist gspec & body] ::args, {:keys [assert-no-body? extern-symbols]} ::opts}]
  (if (and assert-no-body? (seq body))
    (throw (parser-error state "Syntax error: function body not expected!"))
    (let [{::keys [result], ::gr.reg/keys [spec-registry]}
          (-> state                                         ;; NOTE preserves opts
            (assoc ::args gspec)
            (assoc ::result {})
            (gspec-parser arglist))
          lambdas (parse-lambdas body extern-symbols)]
      (-> state
        (update-result assoc-in [::gr.reg/arities (body-arity arglist)]
          (cond-> {::gr.reg/arglist `(quote ~arglist)
                   ::gr.reg/gspec   result}
            (seq body) (assoc ::gr.reg/body `(quote ~(vec body)))))
        (update-result update ::gr.reg/spec-registry merge spec-registry)
        (cond-> (seq lambdas)
          (->                                               ;lambdas
            (update-result assoc ::gr.reg/lambdas
              (utils/map-vals #(dissoc % ::gr.reg/spec-registry) lambdas))
            (update-result update ::gr.reg/spec-registry
              apply merge (map ::gr.reg/spec-registry (vals lambdas)))))
        (next-args nnext)))))

(defn multiple-arities
  [{:as state, ::keys [result args]}]
  (reduce
    (fn [state arity-body]
      (when-not (arity-body? arity-body)
        (throw (parser-error state "Syntax error: multi-arity function body expected!")))
      (-> state
        (assoc ::args arity-body)
        (single-arity)))
    state args))

(defn function-content
  [{:as state [lookahead] ::args}]
  (if (arity-body? lookahead)
    (multiple-arities state)
    (single-arity state)))

(defn parse-defn [args externs]
  (-> (init-parser-state args
        {:extern-symbols (mapv second (keys externs))
         :externs        externs})
    (function-name)
    (optional-docstring)
    (function-content)
    ::result))

(defn parse-fdef [args externs]
  (-> (init-parser-state args
        {:assert-no-body? true
         :extern-symbols  (mapv second (keys externs))
         :externs         externs})
    (var-name)
    (function-content)
    ::result))

(defn parse-fn [args extern-symbols]
  (-> (init-parser-state args
        {:extern-symbols extern-symbols})
    (lambda-name)
    (function-content)
    ::result))

(defn parse-fspec [args]
  (when (symbol? (first args))
    (throw (ex-info "Should not contain a function name, expected an arglist!" {})))
  (-> (init-parser-state args
        {:assert-no-body? true})
    (function-content)
    ::result))
