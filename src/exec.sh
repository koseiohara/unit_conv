#!/bin/bash

#PBS -q tqueue
#PBS -N unit_conv
#PBS -j oe
#PBS -l nodes=1:ppn=1

ulimit -s unlimited

NOW=$(date "+%Y%m%d_%H%M%S")
RESULT_FILE="../output/result_${NOW}.txt"

cd /mnt/jet11/kosei/mim/energetics/hourly_clim/src

./EXE >& ${RESULT_FILE}


