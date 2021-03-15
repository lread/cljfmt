(ns bench
  (:require [cljfmt.core-test]
            [clojure.test :as t]))

(defn bench [desc iterations]
  (println (str "=" desc "=>") iterations "iterations")
  (simple-benchmark [] (t/run-all-tests #"cljfmt.core-test") iterations))

(defn -main []
  (bench "warm up" 3) 
  (bench "run" 100))

(set! *main-cli-fn* -main)
