#!/bin/bash
# A one-time script to upload packages that all instances will need.

# Before running, compile and run 'gsutil mb gs://aorta' to make the bucket

# Stop the script if anything breaks
set -e

# Package up AORTA
echo Packaging AORTA...
rm -f aorta.tgz
tar czf aorta.tgz `find * | grep -v '^logs' | grep -v '^plots' | grep -v scenarios/savestate | grep -v '^final' | grep -v '^osm' | xargs`

# Stick it on GS
gsutil rm -f gs://aorta/aorta.tgz
gsutil cp `pwd`/aorta.tgz gs://aorta
rm -f aorta.tgz
