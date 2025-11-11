(ns com.fulcrologic.guardrails.malli.fulcro-spec-helpers
  "Fulcro spec includes when-mocking! and provided!, which do stub checking via Clojure Spec. This ns has alternatives
   that work for when you are stubbing functions that use guardrails with either Malli or Clojure Spec."
  #?(:cljs (:require-macros [com.fulcrologic.guardrails.malli.fulcro-spec-helpers]))
  (:require
    #?@(:clj  [[fulcro-spec.provided :as fsp]
               [fulcro-spec.spec :as ffs]
               [fulcro-spec.stub :as stub]
               [fulcro-spec.impl.macros :as im]
               [clojure.spec.alpha :as s]]
        :cljs [[cljs.spec.alpha :as s]])
    [com.fulcrologic.guardrails.malli.registry :as gr.reg]
    [malli.core :as mc]))

#?(:clj (def original-conformed-stub fsp/conformed-stub))

(defn -function-info [schema options]
  (when (= (mc/type schema options) :=>)
    (let [[input output guard] (mc/-children schema)
          {:keys [min max]} (mc/-regex-min-max input false)]
      (cond-> {:min    min
               :arity  (if (= min max) min :varargs)
               :input  input
               :output output}
        guard (assoc :guard guard)
        max (assoc :max max)))))

(defn instrument! [f schema report]
  (case (mc/type schema {:registry gr.reg/registry})
    :=> (let [{:keys [min max input output guard]} (-function-info schema {:registry gr.reg/registry})
              scope #{:input :output :guard}
              [validate-input validate-output validate-guard] (mc/-vmap
                                                                (fn [schema]
                                                                  (fn [v] (mc/validate schema v {:registry gr.reg/registry})))
                                                                [input output (or guard :any)])
              [wrap-input wrap-output wrap-guard] (mc/-vmap #(contains? scope %) [:input :output :guard])]
          (fn [& args]
            (let [args (vec args), arity (count args)]
              (when wrap-input
                (when-not (<= min arity (or max 1000))
                  (report ::invalid-arity {:arity arity, :arities #{{:min min :max max}}, :args args, :input input, :schema schema}))
                (when-not (validate-input args)
                  (report ::invalid-input {:input input, :args args, :schema schema})))
              (let [value (apply f args)]
                (when (and wrap-output (not (validate-output value)))
                  (report ::invalid-output {:output output, :value value, :args args, :schema schema}))
                (when (and wrap-guard (not (validate-guard [args value])))
                  (report ::invalid-guard {:guard guard, :value value, :args args, :schema schema}))
                value))))
    :function (let [arity->info  (->> (mc/children schema {:registry gr.reg/registry})
                                   (map (fn [s] (assoc (-function-info s {:registry gr.reg/registry}) :f (instrument! f s report))))
                                   (mc/-group-by-arity!))
                    arities      (-> arity->info keys set)
                    varargs-info (arity->info :varargs)]
                (if (= 1 (count arities))
                  (-> arity->info first val :f)
                  (fn [& args]
                    (let [arity        (count args)
                          {:keys [input] :as info} (arity->info arity)
                          report-arity #(report ::invalid-arity {:arity arity, :arities arities, :args args, :input input, :schema schema})]
                      (cond
                        info (apply (:f info) args)
                        varargs-info (if (< arity (:min varargs-info)) (report-arity) (apply (:f varargs-info) args))
                        :else (report-arity))))))))

(defn spec-instrument!
  "Wraps a stub function with Clojure/ClojureScript Spec validation based on the function's fspec.
   Reports validation errors via the provided report function."
  [f fspec-sym report]
  (if-let [fspec (s/get-spec fspec-sym)]
    (let [args-spec (:args fspec)
          ret-spec  (:ret fspec)
          fn-spec   (:fn fspec)]
      (fn [& args]
        (when args-spec
          (when-not (s/valid? args-spec args)
            (report ::invalid-spec-input
              {:spec     args-spec
               :args     args
               :problems (s/explain-data args-spec args)
               :fspec    fspec})))
        (let [value (apply f args)]
          (when ret-spec
            (when-not (s/valid? ret-spec value)
              (report ::invalid-spec-output
                {:spec     ret-spec
                 :value    value
                 :args     args
                 :problems (s/explain-data ret-spec value)
                 :fspec    fspec})))
          (when fn-spec
            (let [fn-check-input {:args args :ret value}]
              (when-not (s/valid? fn-spec fn-check-input)
                (report ::invalid-spec-fn
                  {:spec     fn-spec
                   :value    value
                   :args     args
                   :problems (s/explain-data fn-spec fn-check-input)
                   :fspec    fspec}))))
          value)))
    ;; If no fspec found, return the function unchanged
    f))

#?(:clj
   (defn conformed-stub [env sym arglist result]
     (let [;; Determine if we're in ClojureScript context at macro expansion time
           cljs?    (:ns env)
           spec-get (if cljs?
                      'cljs.spec.alpha/get-spec
                      'clojure.spec.alpha/get-spec)]
       `(let [stub# (fn [~@arglist] ~result)]
          (cond
            ;; Check for Malli schema
            (get (meta (var ~sym)) :malli/schema)
            (let [schema# (get (meta (var ~sym)) :malli/schema)]
              (instrument! stub# schema#
                (fn [& args#]
                  (throw (ex-info (str "Test setup failure: Your mock of " ~(str sym) " failed to follow the Malli schema of the function.")
                           {:problems args#})))))

            ;; Check for Spec fspec
            ;; Note: (var ~sym) works in both Clojure (returns var) and ClojureScript (returns symbol)
            (~spec-get (var ~sym))
            (spec-instrument! stub# (var ~sym)
              (fn [& args#]
                (throw (ex-info (str "Test setup failure: Your mock of " ~(str sym) " failed to follow the Spec fspec of the function.")
                         {:problems args#}))))

            ;; Fall back to original behavior
            :else
            ~(original-conformed-stub env sym arglist result))))))

#?(:clj
   (defn provided*
     [env conform-mocks? string forms]
     (with-redefs [fsp/conformed-stub conformed-stub]
       (let [{:keys [mocks body]} (ffs/conform! :fulcro-spec.provided/mocks forms)
             scripts      (fsp/parse-mocks env conform-mocks? mocks)
             skip-output? (= :skip-output string)]
         `(im/with-reporting ~(when-not skip-output? {:type :provided :string (str "PROVIDED: " string)})
            (im/try-report "Unexpected"
              (let [~@(mapcat (juxt :symgen :script) scripts)]
                ~@(map
                    (fn [s] `(im/begin-reporting ~{:type :behavior :string s}))
                    (distinct (keep :behavior scripts)))
                (with-redefs [~@(mapcat (juxt :mock-name :sstub) scripts)]
                  (let [result# (binding [stub/*script-by-fn*
                                          ~(into {}
                                             (map (juxt
                                                    #(fsp/emit-mock-fn env (:mock-name %))
                                                    :symgen))
                                             scripts)]
                                  ~@body)]
                    (stub/validate-target-function-counts ~(mapv :symgen scripts))
                    ~@(map
                        (fn [s] `(im/end-reporting ~{:type :behavior :string s}))
                        (distinct (keep :behavior scripts)))
                    result#)))))))))

#?(:clj
   (defmacro provided!
     "Just like `provided`, but forces mocked functions to conform to the guardrails spec of the original function.
     Works with both Malli schemas (via :malli/schema metadata) and Clojure Spec fspecs (via s/fdef).
     Each mocked function is checked individually - if it has a Malli schema, Malli validation is used;
     if it has a Spec fspec, Spec validation is used; otherwise falls back to standard provided behavior."
     [description & forms]
     (provided* &env true description forms)))

#?(:clj
   (defmacro when-mocking!
     "Just like when-mocking, but forces mocked functions to conform to the guardrails spec of the original function.
     Works with both Malli schemas (via :malli/schema metadata) and Clojure Spec fspecs (via s/fdef).
     Each mocked function is checked individually - if it has a Malli schema, Malli validation is used;
     if it has a Spec fspec, Spec validation is used; otherwise falls back to standard when-mocking behavior."
     [& forms]
     (provided* &env true :skip-output forms)))
