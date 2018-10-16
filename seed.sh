#! /bin/sh
#
# Seed local development environment by downloading some of the core packages
# from production instance.
#
# Dependencies for this script:
# - curl
# - jq

SITE=https://package.elm-lang.org
PACKAGES=(elm/core elm/html elm/json elm/browser elm/url elm/http)

PACKAGE_FILES=(elm.json README.md docs.json endpoint.json time.dat)

for p in ${PACKAGES[*]}; do
  echo $p
  mkdir -p packages/$p
  pushd packages/$p
  curl -O $SITE/packages/$p/releases.json
  for v in $(jq -r 'keys[]' < releases.json); do
    mkdir $v
    pushd $v
    for f in ${PACKAGE_FILES[*]}; do
      curl -O $SITE/packages/$p/$v/$f
    done
    popd
  done
  popd
done