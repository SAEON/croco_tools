#!/bin/bash  

source ../myenv_mypath.sh
set -u
set +x     # pour CURIE
umask 022

#-------------------------------------------------------------------------------
#  namelist of the experiment
#-------------------------------------------------------------------------------
#cat mypath.sh >> mynamelist.tmp
cat mynamelist.sh > mynamelist.tmp
cat myjob.sh >> mynamelist.tmp
cat ./routines/common_definitions.sh >> mynamelist.tmp

. ./mynamelist.tmp

[ ! -d ${JOBDIR_ROOT} ] && mkdir -p ${JOBDIR_ROOT}  # for the first submitjob.sh call

cd ${JOBDIR_ROOT} 
ls ${jobname}  > /dev/null  2>&1 
if [ "$?" -eq "0" ] ; then
   if [ ${CHAINED_JOB} == "FALSE" ]; then 
       printf "\n\n\n\n  Un fichier ${jobname} existe deja  dans  ${JOBDIR_ROOT} \n             => exit. \n\n  Nettoyer et relancer\n\n\n\n"; exit
   elif [ ${CHAINED_JOB} == "TRUE" ] && [ ${DATE_BEGIN_JOB} -eq ${DATE_BEGIN_EXP} ]; then
       printf "\n\n\n\n  Un fichier ${jobname} existe deja  dans  ${JOBDIR_ROOT} \n             => exit. \n\n  Nettoyer et relancer\n\n\n\n"; exit
   fi
      
fi
cd -
#-------------------------------------------------------------------------------
# calendar computations (to check dates consistency)
#-------------------------------------------------------------------------------
if [ ${USE_CPL} -ge 1 ]; then
  if [ $(( ${CPL_FREQ} % ${TSP_ATM} )) -ne 0 ] || \
     [ $(( ${CPL_FREQ} % ${TSP_OCE} )) -ne 0 ] || \
     [ $(( ${CPL_FREQ} % ${TSP_WAV} )) -ne 0 ] ; then
     printf "\n\n Problem of consistency between Coupling Frequency and Time Step with ATM, OCE or WAV model, we stop...\n\n" && exit 1
  fi
  if [ ${USE_TOY} -eq 1 ]; then 
      for k in `seq 0 $(( ${nbtoy} - 1))` ; do
          if [ $(( ${CPL_FREQ} % ${TSP_TOY[$k]} )) -ne 0 ] ; then
              printf "\n\n Problem of consistency between Coupling Frequency and Time Step for TOY model, we stop...\n\n" && exit 1
          fi
      done
  fi
fi

. ${SCRIPTDIR}/routines/caltools.sh

#-------------------------------------------------------------------------------
# create job and submit it
#-------------------------------------------------------------------------------

[ ${USE_OCE}  -eq 1 ] && TOTOCE=$(( $NP_OCEX * $NP_OCEY )) || TOTOCE=0
[ ${USE_ATM}  -eq 1 ] && TOTATM=$NP_ATM  || TOTATM=0
[ ${USE_WAV}  -eq 1 ] && TOTWAV=$NP_WAV  || TOTWAV=0
[ ${USE_TOY}  -eq 1 ] && { for k in `seq 0 $(( ${nbtoy} - 1))`; do TOTTOY+=$NP_TOY ; done;}  || TOTTOY=0
[ ${USE_XIOS_ATM} -eq 1 ] && TOTXIO=$NP_XIOS_ATM || TOTXIO=0
[ ${USE_XIOS_OCE} -eq 1 ] && TOTXIO=$(( ${TOTXIO} + ${NP_XIOS_OCE} ))
totalcore=$(( $TOTOCE + $TOTATM + $TOTWAV + $TOTTOY + $TOTXIO ))

[ ${COMPUTER} == "DATARMOR" ] && nbnode=$(( $totalcore /29 +1)) || nbnode=0


if [ ${MACHINE} == "IRENE" ]; then
    timedur=${TIMEJOB}
else
    timedur=$( sec2hour ${TIMEJOB} )
fi

sed -e "/< insert here variables definitions >/r mynamelist.tmp" \
    -e "s/<exp>/${ROOT_NAME_1}/g" \
    -e "s/<nbnode>/${nbnode}/g" \
    -e "s/<nmpi>/${totalcore}/g" \
    -e "s/<projectid>/${projectid}/g" \
    -e "s/<timedur>/${timedur}/g" \
    ./routines/header.${COMPUTER} > HEADER_tmp
    cat HEADER_tmp ./routines/job.base.sh >  ${JOBDIR_ROOT}/${jobname}
    \rm HEADER_tmp
    \rm ./mynamelist.tmp


cd ${JOBDIR_ROOT}
chmod 755 ${jobname}

printf "\n  HOSTNAME: `hostname`\n     =>    COMPUTER: ${COMPUTER}\n"  
echo
printf "  CONFIG: ${CONFIG}\n"  
printf "  CEXPER: ${CEXPER}\n"  
echo
printf "  jobname: ${jobname}\n"  
echo
printf "  ROOT_NAME_1: ${ROOT_NAME_1}\n"  
printf "  ROOT_NAME_2: ${ROOT_NAME_2}\n"  
printf "  ROOT_NAME_3: ${ROOT_NAME_3}\n"  
printf "  EXEDIR: ${EXEDIR_ROOT}\n"  
printf "  OUTPUTDIR: ${OUTPUTDIR_ROOT}\n"  
printf "  RESTDIR_OUT: ${RESTDIR_ROOT}\n"  
printf "  JOBDIR: ${JOBDIR_ROOT}\n"  

if [ "${SCRIPT_DEBUG}" == "TRUE" ] ; then
   printf "\n\n\n\n  SCRIPT_DEBUG=${SCRIPT_DEBUG}  Mode script debug => Pas de soumission en queue\n\n\n\n"
else 
    if [ ${CHAINED_JOB} == "TRUE" ]; then
        [ ${DATE_BEGIN_JOB} -eq ${DATE_BEGIN_EXP} ] && . ${SCRIPTDIR}/routines/chained_job.sh

    else
       ${QSUB} ${jobname}
    fi 
#
    if [ "${MODE_TEST}" != "" ] ; then
        printf "\n\n\n\n  MODE_TEST=${MODE_TEST}  Mode test et non production => Pas d'enchainement de jobs.\n\n\n\n"
    fi
fi

