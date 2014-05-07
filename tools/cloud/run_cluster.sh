#!/bin/bash
# Script to bring up a cluster

NUM=16

GCUTIL='gcutil addinstance router-{} --zone="us-central1-a" --machine_type="n1-standard-1" --service_account_scopes="https://www.googleapis.com/auth/compute,https://www.googleapis.com/auth/devstorage.full_control" --persistent_boot_disk="false" --metadata_from_file=startup-script:`pwd`/tools/cloud/startup_script.sh --image=projects/debian-cloud/global/images/debian-7-wheezy-v20130816'

parallel -j10 --tag --linebuffer $GCUTIL ::: `seq 1 $NUM`
