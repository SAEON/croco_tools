#!/bin/bash
#
#
# 1/1/1993 (jours depuis 1990/1/1)
#
T_START=1096
#
# 25/12/2018 (jours depuis 1990/1/1)
#
T_END=1097
T_END=10585
#
DIR_OUT=/media/ppenven/GLORYS12/NC/
PREFIX=GLORYS_12V1_SAFE_
SUFFIX=.nc
#
# Main Loop
#
NJOB=0
MAXJOB=1
T=$T_START
T_END=$((T_END + 1))
while [  $T -lt $T_END ]; do

   if [[ ${T} -lt 10 ]]; then
     TOUT=00000${T}
   elif [[ ${T} -lt 100 ]]; then
     TOUT=0000${T}
   elif [[ ${T} -lt 1000 ]]; then
     TOUT=000${T}
   elif [[ ${T} -lt 10000 ]]; then
     TOUT=00${T}
   elif [[ ${T} -lt 100000 ]]; then
     TOUT=0${T}
   fi
   FNAME=${DIR_OUT}${PREFIX}${TOUT}${SUFFIX}

   if [ -f "$FNAME" ]; then
     echo "$FNAME exists"
   else
     echo "$FNAME does not exist"
     python -u ./glorys_opendap_extract_index.py $T > batch_${T}.out &
     let NJOB=NJOB+1
     if [[ ${NJOB} -ge ${MAXJOB} ]]; then
       echo "wait "$NJOB
       wait
       NJOB=0
     fi
   fi
 let T=T+1 
done

