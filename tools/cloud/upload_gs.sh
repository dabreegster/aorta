#!/bin/bash
# Upload a file to Google Storage

FN=$1
STRING=$2

echo -e $STRING | cat > foo
echo -e $STRING | gsutil cp - $FN
