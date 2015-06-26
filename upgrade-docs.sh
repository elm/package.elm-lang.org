#!/bin/bash
PACKAGES=packages/*/*/*
for pkg in $PACKAGES
do
  echo "$pkg"
  ./dist/build/upgrade-docs/upgrade-docs $pkg
done