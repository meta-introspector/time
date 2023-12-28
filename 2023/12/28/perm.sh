#!/bin/bash

GIT_URL=$(git config --local remote.origin.url | cut -d "@" -f 2 | cut -d "." -f "1-2" | sed "s/:/\//")
URL="${GIT_URL}"
BRANCH=`git rev-parse --abbrev-ref HEAD`
FILE=$1

echo $URL/blob/$BRANCH/$1

exit 0
