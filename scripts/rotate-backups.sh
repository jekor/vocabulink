#!/bin/bash

PREFIX="vocabulink.com-archive/vocabulink--"
SUFFIX=".sql.gpg"

# List the backup dates.
for f in `s3cmd ls s3://$PREFIX | grep $SUFFIX | cut -c 1-10 | $HOME/.cabal/bin/log2rotate --delete`; do
    s3cmd del s3://$PREFIX$f$SUFFIX
done