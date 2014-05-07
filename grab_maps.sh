#!/bin/bash

mkdir -p osm
mkdir -p maps
mkdir -p scenarios

wget -r -c -np --user-agent="Mozilla/5.0 (Windows NT 5.2; rv:2.0.1) Gecko/20100101 Firefox/4.0.1" http://www.aorta-traffic.org/aorta_data/
mv -v www.aorta-traffic.org/aorta_data/osm/*.osm osm/
mv -v www.aorta-traffic.org/aorta_data/maps/*.map maps/
rm -rf www.aorta-traffic.org
