(ns com.fulcrologic.guardrails.impl.externs-spec
  (:require
    [com.fulcrologic.guardrails.impl.externs :as gr.externs]
    [fulcro-spec.core :refer [specification assertions when-mocking]]))

(specification "extern-symbols"
  (when-mocking
    (gr.externs/cljc-resolve _ s) => {::MOCK ::VALUE}
    (assertions
      (gr.externs/extern-symbols {}
        '[(foo (bar) 0) (qux 1 [2 extern/three])])
      => (zipmap [''foo ''bar ''qux ''extern/three]
           (repeat {::MOCK ::VALUE})))))
