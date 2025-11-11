(ns limited-mcp
  (:require
    [clojure-mcp.core :as core]
    [clojure-mcp.main :as main]
    [clojure-mcp.tools :as tools]))

(def desired-tools [:clojure_inspect_project :clojure_eval :clojure_edit :clojure_edit_replace_sexp :clojurescript_eval])

(defn make-tools
  [nrepl-client-atom _working-directory]
  (let [all-tools (vec (concat
                         (tools/build-eval-tools nrepl-client-atom)
                         (tools/build-editing-tools nrepl-client-atom)))]
    (tools/filter-tools all-tools desired-tools nil)))

(defn start-mcp-server
  [opts]
  (core/build-and-start-mcp-server
    opts
    {:make-tools-fn     make-tools
     :make-prompts-fn   main/make-prompts                   ; Keep standard prompts
     :make-resources-fn main/make-resources                 ; Keep standard resources
     }))
