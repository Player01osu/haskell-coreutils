#!/bin/sh

[ -d ./target ] || mkdir target

ghc -o target/cat src/cat.hs -no-keep-hi-files -no-keep-o-files -O2
ghc -o target/tac src/tac.hs -no-keep-hi-files -no-keep-o-files -O2
ghc -o target/ls src/ls.hs -no-keep-hi-files -no-keep-o-files -O2
