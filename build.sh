#!/bin/bash

set -e


## DOWNLOAD BINARIES

PATH=$(pwd)/node_modules/.bin:$PATH

if ! [ -x "$(command -v elm)" ]; then
  npm install elm@latest-0.19.1
fi
if ! [ -x "$(command -v uglifyjs)" ]; then
  npm install uglify-js
fi


## BUILD FRONTEND

elm make src/frontend/Main.elm --optimize --output=artifacts/elm.js >/dev/null

HASH=$(shasum artifacts/elm.js | cut -b 1-40)

if [ ! -f "artifacts/$HASH" ]
then
  uglifyjs artifacts/elm.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" \
    | uglifyjs --mangle \
    | gzip -9 > "artifacts/$HASH"
fi

printf $HASH
