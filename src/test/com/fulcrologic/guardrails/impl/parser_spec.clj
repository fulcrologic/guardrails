(ns com.fulcrologic.guardrails.impl.parser-spec
  (:require
    [com.fulcrologic.guardrails.registry :as gr.reg]
    [com.fulcrologic.guardrails.impl.parser :as gr.parser]
    [fulcro-spec.check :as _]
    [fulcro-spec.core :refer [specification assertions when-mocking]]))

(specification "parse-defn"
  (when-mocking
    (gr.parser/location-of-lambda _) => [:LINE :COL]
    (assertions
      (gr.parser/parse-defn '(foo [x] [int? => int?] (str x)) [])
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
                               ((>fn [x] [int? => string?] (str x)) x)) [])
      =check=>
      (_/embeds?*
        #::gr.reg{:fn-name ''foo
                  :lambdas {[:LINE :COL]
                            #::gr.reg{:env->fn (_/is?* seq?)
                                      ;; FIXME: fixed on develop, dont need equals?*
                                      :spec-registry (_/equals?* ::_/not-found)}}
                  :spec-registry {''int? 'int?, ''string? 'string?}}))))
