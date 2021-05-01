(ns perf
  (:require [cljfmt.core :as cljfmt]
            [criterium.core :as criterium]
            [rewrite-clj.parser :as p]))

(def sample1 "https://raw.githubusercontent.com/clojure-lsp/clojure-lsp/cd64d430d83cb068d4a0a011beeded3beb791201/src/clojure_lsp/handlers.clj")

(defn -main [[test-id]]
  (case test-id
    "reformat-string"
    (let [s (slurp sample1)]
      (println "benchmarking reformat-string")
      (criterium/with-progress-reporting (criterium/bench (cljfmt/reformat-string s) :verbose)))

    "reformat-form"
    (let [s (slurp sample1)
          n (p/parse-string-all s)]
      (println "benchmarking reformat-form")
      (criterium/with-progress-reporting (criterium/bench (cljfmt/reformat-form n) :verbose)))))

(-main *command-line-args*)
