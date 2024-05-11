(ns com.fulcrologic.guardrails.malli.fulcro-spec-helpers
  "Fulcro spec includes when-mocking! and provided!, which do stub checking via Clojure Spec. This ns has alternatives
   that work for when you are stubbing functions that use Malli."
  #?(:cljs (:require-macros [com.fulcrologic.guardrails.malli.fulcro-spec-helpers]))
  #?(:clj
     (:require
       [fulcro-spec.impl.macros :as im]
       [fulcro-spec.provided :as fsp]
       [fulcro-spec.spec :as ffs]
       [fulcro-spec.stub :as stub]
       [malli.core :as mc])))

#?(:clj
   (defn conformed-stub [env sym arglist result]
     `(let [stub# (fn [~@arglist] ~result)]
        (if-let [schema# (get (meta (var ~sym)) :malli/schema)]
          (mc/-instrument
            {:schema schema#
             :report (fn [& args#]
                       (throw (ex-info (str "Test setup failure: Your mock of " ~(str sym) " failed to follow the schema of the function.")
                                {:problems args#})))}
            stub#)))))

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
