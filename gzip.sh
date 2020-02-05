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

#                         # total savings
#                         # 5 Feb 2020
#                         #------------
batchGzip "elm.json"      #   2,460,384
batchGzip "docs.json"     # 204,728,592
batchGzip "README.md"     #  11,402,288
batchGzip "releases.json" #      94,960
#                         #------------
#                         # 218,686,224

# before:  269,159,000
# after:    50,472,776
