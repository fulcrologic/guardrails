(ns ^:no-doc com.fulcrologic.guardrails.impl.protocol
  "INTERNAL. Macro-time syntax support for `>defprotocol`: parsing, validation, and synthesis of the plain
   `defprotocol` form. Has no dependencies so that the `noop` namespace can use it.")

(def ^:private ret-or-such-that? #{'=> :ret '| :st})

(defn gspec-arg-count
  "Returns the number of argument specs in `gspec` (everything before the first `=>`, `:ret`, `|`, or `:st`)."
  [gspec]
  (count (take-while (complement ret-or-such-that?) gspec)))

(defn arglist
  "Returns the synthesized arglist `[this arg1 arg2 ...]` for `gspec`."
  [gspec]
  (into ['this] (map #(symbol (str "arg" %))) (range 1 (gspec-arg-count gspec))))

(defn- syntax-error [psym msg data]
  (ex-info (str ">defprotocol " psym ": " msg) (assoc data :protocol psym)))

(defn- validate-method!
  "Throws a macro-time error if the parsed `method` of protocol `psym` is malformed."
  [psym {:keys [name gspecs]}]
  (when-not (simple-symbol? name)
    (throw (syntax-error psym (str "Method name must be a simple symbol, got " (pr-str name)) {:method name})))
  (when (empty? gspecs)
    (throw (syntax-error psym (str "Method " name " has no gspec. Each arity is declared with a gspec like "
                                   "[::Proto arg-spec* => ret-spec].")
                         {:method name})))
  (doseq [g gspecs]
    (when (some #{'&} g)
      (throw (syntax-error psym (str "Method " name " has a variadic gspec " (pr-str g)
                                     ". Protocol methods cannot be variadic.")
                           {:method name :gspec g})))
    (when-not (some #{'=> :ret} g)
      (throw (syntax-error psym (str "Method " name " has a gspec without a return spec (=>): " (pr-str g))
                           {:method name :gspec g})))
    (when (zero? (gspec-arg-count g))
      (throw (syntax-error psym (str "Method " name " has a gspec with no argument specs: " (pr-str g)
                                     ". The first argument spec describes `this`.")
                           {:method name :gspec g}))))
  (let [arities (map gspec-arg-count gspecs)
        dups    (into (sorted-set)
                      (keep (fn [[n c]] (when (> c 1) n)))
                      (frequencies arities))]
    (when (seq dups)
      (throw (syntax-error psym (str "Method " name " declares more than one gspec for arity " (first dups)
                                     " (counting `this`).")
                           {:method name :arities (vec dups)})))))

(defn parse-body
  "Parses the `body` of `(>defprotocol psym & body)` and validates it, throwing a macro-time `ex-info` on bad syntax.
   Returns:

   * `:doc` - The protocol docstring, if any.
   * `:opts` - A vector of the protocol options (e.g. `[:extend-via-metadata true]`), in order.
   * `:methods` - A vector of `{:name sym :gspecs [gspec ...] :doc str?}`, one per method."
  [psym body]
  (when-not (simple-symbol? psym)
    (throw (ex-info (str ">defprotocol name must be a simple symbol, got " (pr-str psym)) {:protocol psym})))
  (let [[doc body] (if (string? (first body)) [(first body) (rest body)] [nil body])]
    (loop [acc   {:doc doc :opts [] :methods []}
           items body]
      (let [[head & tail] items]
        (cond
          (empty? items) (do
                           (when-let [dup (some (fn [[n c]] (when (> c 1) n))
                                                (frequencies (map :name (:methods acc))))]
                             (throw (syntax-error psym (str "Method " dup " is declared more than once.") {:method dup})))
                           (run! (partial validate-method! psym) (:methods acc))
                           acc)
          (keyword? head) (if (seq tail)
                            (recur (update acc :opts conj head (first tail)) (rest tail))
                            (throw (syntax-error psym (str "Option " head " is missing a value.") {:option head})))
          (seq? head) (let [[mname & sigs] head
                            bad (remove #(or (vector? %) (string? %)) sigs)]
                        (when (seq bad)
                          (throw (syntax-error psym (str "Method " mname " may only contain gspec vectors and an "
                                                         "optional docstring, but found " (pr-str (first bad)))
                                               {:method mname})))
                        (recur (update acc :methods conj (cond-> {:name   mname
                                                                  :gspecs (filterv vector? sigs)}
                                                           (some string? sigs) (assoc :doc (first (filter string? sigs)))))
                               tail))
          :else (throw (syntax-error psym (str "Unexpected form " (pr-str head)) {:form head})))))))

(defn defprotocol-form
  "Returns the plain `(clojure.core/defprotocol ...)` form for a parsed protocol, with arglists synthesized from the
   gspecs."
  [psym {:keys [doc opts methods]}]
  `(defprotocol ~psym ~@(when doc [doc]) ~@opts
                ~@(map (fn [{:keys [name gspecs doc]}]
                         `(~name ~@(map arglist gspecs) ~@(when doc [doc])))
                       methods)))

(defn method-defn-forms
  "Returns `>defn`-style forms `(name doc? ([this arg1 ..] gspec body-form) ...)` for a parsed `method`, where
   `body-fn` is called with the synthesized arglist (and arity count) to produce the body of each arity."
  [{:keys [name gspecs doc]} body-fn]
  (list* name
         (concat (when doc [doc])
                 (map (fn [gspec]
                        (let [args (arglist gspec)]
                          (list args gspec (body-fn args))))
                      gspecs))))

(defn satisfies-pred
  "Returns a `(fn [x] ...)` form that is true when `x` implements protocol `psym` (defined in namespace `nsname`),
   including implementations supplied via metadata when the protocol is `:extend-via-metadata`."
  [psym nsname {:keys [opts methods]}]
  (let [x         (gensym "x")
        via-meta? (some #{:extend-via-metadata} (take-nth 2 opts))]
    `(fn [~x] (boolean (or (satisfies? ~psym ~x)
                           ~@(when via-meta?
                               (map (fn [{:keys [name]}]
                                      `(contains? (meta ~x) '~(symbol (str nsname) (str name))))
                                    methods)))))))
