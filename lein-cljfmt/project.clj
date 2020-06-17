(defproject lein-cljfmt "0.6.7"
  :description "A library for formatting Clojure code"
  :url "https://github.com/weavejester/cljfmt"
  :scm {:dir ".."}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :eval-in-leiningen true
  :dependencies [[cljfmt
                  ;; I think specifying the branch name instead of the SHA should work
                  "lread-rewrite-cljc-test"]]
  :plugins [[reifyhealth/lein-git-down "0.3.6"]]
  :middleware [lein-git-down.plugin/inject-properties]
  :repositories [["public-github" {:url "git://github.com"}]]
  :git-down {cljfmt {:coordinates lread/cljfmt
                     :manifest-root "cljfmt"}})
