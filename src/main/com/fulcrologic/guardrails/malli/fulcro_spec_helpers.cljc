(ns com.fulcrologic.guardrails.malli.fulcro-spec-helpers
  "Fulcro spec includes when-mocking! and provided!, which do stub checking via Clojure Spec. This ns has alternatives
   that work for when you are stubbing functions that use Malli."
  #?(:cljs (:require-macros [com.fulcrologic.guardrails.malli.fulcro-spec-helpers]))
  #?(:clj
     (:require
       [com.fulcrologic.guardrails.malli.registry :as gr.reg]
       [fulcro-spec.impl.macros :as im]
       [fulcro-spec.provided :as fsp]
       [fulcro-spec.spec :as ffs]
       [fulcro-spec.stub :as stub]
       [malli.core :as mc])))

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

#?(:clj
   (defn conformed-stub [env sym arglist result]
     `(let [stub# (fn [~@arglist] ~result)]
        (if-let [schema# (get (meta (var ~sym)) :malli/schema)]
          (do
            (instrument! stub# schema#
              (fn [& args#]
                (throw (ex-info (str "Test setup failure: Your mock of " ~(str sym) " failed to follow the schema of the function.")
                         {:problems args#})))))
          ~(original-conformed-stub env sym arglist result)))))

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
     "Just like `provided`, but forces mocked functions to conform to the malli spec of the original function (if available)."
     [description & forms]
     (provided* &env true description forms)))

#?(:clj
   (defmacro when-mocking!
     "Just like when-mocking, but forces mocked functions to conform to the malli spec of the original function (if available)."
     [& forms]
     (provided* &env true :skip-output forms)))
