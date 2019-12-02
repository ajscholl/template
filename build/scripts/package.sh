#!/bin/bash

set -e

cd "$(dirname "$0")"
cd ../..

if [ -d output ]; then
  rm -rf output
fi

mkdir -p output

if [ -d .stack-work ]; then
  rm -rf .stack-work
fi

stack build
LIBRARY=$(find .stack-work/install/ -name libtemplates-to-go-runtime.so)

for lib in $(ldd -v "$LIBRARY" | grep -v ' => ' | grep ".so" | sed 's/^\s*//' | sed 's/:$//'); do
  if [ -e "$lib" ]; then
    cp --dereference "$lib" output/
  fi
done

LD_LIBRARY_PATH=output ldd output/libtemplates-to-go-runtime.so
