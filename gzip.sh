#!/bin/bash

set -e


function batchGzip {
    for file in $(find packages -type f -name $1)
    do
        file_gz="$file.gz"
        if [ ! -f $file_gz ]; then
            gzip -9 < $file > $file_gz
        fi
    done
}

batchGzip "elm.json"
batchGzip "docs.json"
batchGzip "README.md"
