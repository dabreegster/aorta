#!/bin/bash
# Startup script each instance executes to setup packages and launch the script to run experiments.

GS=gs://aorta/`hostname`

# TODO update to use sbt

# Install java and scala
sudo apt-get update
sudo apt-get install -y openjdk-6-jre libjansi-java less
wget http://scala-lang.org/files/archive/scala-2.10.2.deb
sudo dpkg -i scala-2.10.2.deb

# Grab the AORTA package
gsutil cp gs://aorta/aorta.tgz .
mkdir aorta
cd aorta
tar xzf ../aorta.tgz
./tools/cloud/upload_gs.sh ${GS}-a-status 'Compiling'
./recompile

# Run the experiment
for iter in a b c d e
do
  #./tools/analyze_routes cloud ${GS}-${iter}-
  #./tools/analyze_externality cloud ${GS}-${iter}-
  ./tools/clowncar_experiment cloud ${GS}-${iter}-
  #./tools/auction_experiment cloud ${GS}-${iter}-
done

# When we're done, shutdown
gcutil deleteinstance --force `hostname`
