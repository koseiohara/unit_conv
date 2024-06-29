#!/bin/bash

#PBS -q tqueue
#PBS -N unit_conv
#PBS -j oe
#PBS -l nodes=1:ppn=1

ulimit -s unlimited

VAR=LRGHR
NOW=$(date "+%Y%m%d_%H%M%S")
RESULT="../output/result_${VAR}_${NOW}.txt"
NML="../nml/input_${VAR}.nml"

cd /mnt/jet11/kosei/Fortran/unit_conv/src

./unit_conv < ${NML} >& ${RESULT}

cat "" >> ${RESULT}
cat ${NML} >> ${RESULT}

