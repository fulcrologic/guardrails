(ns com.fulcrologic.guardrails.impl.parser-spec
  (:require
    [com.fulcrologic.guardrails.registry :as gr.reg]
    [com.fulcrologic.guardrails.impl.parser :as gr.parser]
    [fulcro-spec.check :as _]
    [fulcro-spec.core :refer [specification assertions when-mocking]]))

(specification "resolve-spec"
  (let [test-externs {(quote 'int?)    #::gr.reg{:extern-name `(quote int?)}
                      (quote 's/+)     #::gr.reg{:extern-name `(quote clojure.spec.alpha/+)}
                      (quote 'foo/bar) #::gr.reg{:extern-name `(quote fq.foo/bar)}}]
    (assertions
      (gr.parser/resolve-spec test-externs 'int?)
      => 'clojure.core/int?
      (gr.parser/resolve-spec test-externs 'foo/bar)
      => 'fq.foo/bar
      (gr.parser/resolve-spec test-externs '(s/+ int?))
      => '(clojure.spec.alpha/+ clojure.core/int?)
      (gr.parser/resolve-spec test-externs '(not/found foo/bar))
      => '(not/found fq.foo/bar))))

(specification "parse-defn"
  (when-mocking
    (gr.parser/location-of-lambda _) => [:LINE :COL]
    (assertions
      (gr.parser/parse-defn '(foo [x] [int? => int?] (str x)) {})
      =check=>
      (_/embeds?*
        #::gr.reg{:fn-name ''foo
                  :fn-ref 'foo
                  :arities {1 #::gr.reg{:arglist ''[x]
                                        :gspec #::gr.reg{:argument-types ["int?"]
                                                         :quoted.argument-specs '['int?]
                                                         :return-type "int?"
                                                         :quoted.return-spec ''int?}
                                        :body ''[(str x)]}}
                  :spec-registry {''int? 'int?}})
      (gr.parser/parse-defn '(foo [x] [int? => int?]
                               ((>fn [x] [int? => string?] (str x)) x)) {})
      =check=>
      (_/embeds?*
        #::gr.reg{:fn-name ''foo
                  :lambdas {[:LINE :COL]
                            #::gr.reg{:env->fn (_/is?* seq?)
                                      :spec-registry ::_/not-found}}
                  :spec-registry {''int? 'int?, ''string? 'string?}}))))
