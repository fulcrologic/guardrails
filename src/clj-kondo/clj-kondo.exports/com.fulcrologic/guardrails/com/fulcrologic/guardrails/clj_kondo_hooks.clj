(ns com.fulcrologic.guardrails.clj-kondo-hooks
  (:require [clj-kondo.hooks-api :as api]))

(def =>? #{'=> :ret})
(def |? #{'| :st})
(def known-sym? #{'=> '| '<-})

(defn args+gspec+body [nodes]
  (let [argv      (first nodes)
        gspec     (second nodes)
        body      (nnext nodes)
        gspec'    (->> gspec
                    (:children)
                    (filterv #(-> % :value known-sym? not))
                    (api/vector-node))
        new-nodes (list* argv gspec' body)]
    ;; gspec: [arg-specs* (| arg-preds+)? => ret-spec (| fn-preds+)? (<- generator-fn)?]
    (if (not= 1 (count (filter =>? (api/sexpr gspec))))
      (api/reg-finding! (merge (meta gspec)
                          {:message (str "Gspec requires exactly one `=>` or `:ret`")
                           :type    :clj-kondo.fulcro.>defn/invalid-gspec}))
      (let [p (partition-by (comp not =>? api/sexpr) (:children gspec))
            [arg [=>] [ret-spec & _output]] (if (-> p ffirst api/sexpr =>?)
                                              (cons [] p)   ; arg-specs might be empty
                                              p)
            [arg-specs [| & arg-preds]] (split-with (comp not |? api/sexpr) arg)]

        (when-not ret-spec
          (println =>)
          (api/reg-finding! (merge (meta =>)
                              {:message "Missing return spec."
                               :type    :clj-kondo.fulcro.>defn/invalid-gspec})))

        ;; (| arg-preds+)?
        (when (and | (empty? arg-preds))
          (api/reg-finding! (merge (meta |)
                              {:message "Missing argument predicates after |."
                               :type    :clj-kondo.fulcro.>defn/invalid-gspec})))


        (let [len-argv       (count (remove #{'&} (api/sexpr argv))) ; [a & more] => 2 arguments
              arg-difference (- (count arg-specs) len-argv)]
          (when (not (zero? arg-difference))
            (let [too-many-specs? (pos? arg-difference)]
              (api/reg-finding! (merge
                                  (meta (if too-many-specs?
                                          (nth arg-specs (+ len-argv arg-difference -1)) ; first excess spec
                                          gspec))           ; The gspec is wrong, not the surplus argument.
                                  {:message (str "Guardrail spec does not match function signature. "
                                              "Too " (if too-many-specs? "many" "few") " specs.")
                                   :type    :clj-kondo.fulcro.>defn/invalid-gspec})))))))
    new-nodes))

(defn >defn
  [{:keys [node]}]
  (let [args       (rest (:children node))
        [fn-name second-arg] args
        ?docstring (when (some-> second-arg api/sexpr string?)
                     second-arg)
        ?attr-map  (let [?attr-map (if ?docstring (nth args 2) second-arg)]
                     (when (some-> ?attr-map api/sexpr map?)
                       ?attr-map))
        args       (cond->> (next args)
                     ?docstring next
                     ?attr-map next)
        post-docs  (if (every? #(-> % api/sexpr list?) args)
                     (mapv #(-> % :children args+gspec+body api/list-node) args)
                     (args+gspec+body args))
        post-name  (cond->> post-docs
                     ?docstring (list* ?docstring)
                     ?attr-map (list* ?attr-map))
        new-node   (api/list-node
                     (list*
                       (api/token-node 'defn)
                       fn-name
                       post-name))]
    {:node new-node}))

(def ^:private stop-arg-specs? (into =>? |?))

(defn- amp? [node] (= '& (api/sexpr node)))

(defn- without-amp
  "Returns `gspec` without `&` (variadic gspecs are reported, and the rest of the gspec is still linted)."
  [gspec]
  (with-meta (api/vector-node (vec (remove amp? (:children gspec)))) (meta gspec)))

(defn- gspec-arg-count [gspec]
  (count (take-while (comp not stop-arg-specs? api/sexpr) (:children gspec))))

(defn- protocol-arglist [n]
  (api/vector-node (into [(api/token-node 'this)]
                     (map #(api/token-node (symbol (str "arg" %))))
                     (range 1 n))))

(defn- protocol-method
  "Validates the gspecs of one `>defprotocol` method node and returns
   `{:sig <defprotocol method node> :checks [<fn node per gspec>]}`."
  [method]
  (let [[mname & sigs] (:children method)
        gspecs  (map (fn [gspec]
                       (if-let [amp (first (filter amp? (:children gspec)))]
                         (do
                           (api/reg-finding! (merge (meta amp)
                                               {:message "Variadic gspecs are not supported in >defprotocol."
                                                :type    :clj-kondo.fulcro.>defn/invalid-gspec}))
                           (without-amp gspec))
                         gspec))
                  (filter #(= :vector (api/tag %)) sigs))
        docs    (filter #(string? (api/sexpr %)) sigs)
        seen    (atom #{})
        checks  (mapv (fn [gspec]
                        (let [n    (gspec-arg-count gspec)
                              ;; An empty argv for a gspec without arg specs avoids a second (misleading) finding.
                              argv (if (zero? n) (api/vector-node []) (protocol-arglist n))]
                          (when (zero? n)
                            (api/reg-finding! (merge (meta gspec)
                                                {:message "Protocol method gspec needs at least one argument spec (for `this`)."
                                                 :type    :clj-kondo.fulcro.>defn/invalid-gspec})))
                          (when (contains? @seen n)
                            (api/reg-finding! (merge (meta gspec)
                                                {:message (str "Duplicate gspec for arity " n ".")
                                                 :type    :clj-kondo.fulcro.>defn/invalid-gspec})))
                          (swap! seen conj n)
                          ;; The body references the synthesized bindings, so that they are not reported as unused.
                          (let [[argv gspec'] (args+gspec+body [argv gspec])]
                            (api/list-node [(api/token-node 'fn) argv
                                            (api/vector-node (conj (vec (:children argv)) gspec'))]))))
                  gspecs)]
    (when (empty? gspecs)
      (api/reg-finding! (merge (meta method)
                          {:message "Protocol method requires at least one gspec."
                           :type    :clj-kondo.fulcro.>defn/invalid-gspec})))
    {:sig    (api/list-node (concat [mname]
                              (map (comp protocol-arglist gspec-arg-count) gspecs)
                              docs))
     :checks checks}))

(defn >defprotocol
  "Rewrites `(>defprotocol P doc? opts* (m gspec+ doc?)+)` to a `defprotocol` with `[this argN]` arglists, followed by
   one `(fn [this argN] gspec)` per arity so the gspecs are validated and linted."
  [{:keys [node]}]
  (let [[psym & body] (rest (:children node))
        parsed   (map (fn [n] (if (= :list (api/tag n)) (protocol-method n) {:sig n})) body)
        new-node (api/list-node
                   (list*
                     (api/token-node 'do)
                     (api/list-node (list* (api/token-node 'defprotocol) psym (map :sig parsed)))
                     (mapcat :checks parsed)))]
    {:node new-node}))
