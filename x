#!/bin/sh

[ -d ./target ] || mkdir target

ghc -o target/cat src/cat.hs -no-keep-hi-files -no-keep-o-files -O2
