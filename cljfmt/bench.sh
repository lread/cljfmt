#!/usr/bin/env bash


bench () {
   local version="$1"
   local jsfile="bench-${version}.js"

   echo ">== compiling bench for ${version}"
   rm -rf "${jsfile}"
   clojure "-M:rewrite-clj/${version}:cljs:bench:test" \
    --main cljs.main \
    --target node \
    --optimizations none \
    --output-to "${jsfile}" \
    --compile bench
   echo ">== running bench for ${version}"
   node "${jsfile}"
}

bench v0
bench v1
