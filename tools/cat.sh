#!/bin/bash
# Concatenate files

NAME=$1

cat ${NAME}_* >> $NAME
rm -f ${NAME}_*
