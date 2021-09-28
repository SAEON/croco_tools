#!/bin/ksh
set -ue
#set -vx
#
. ${SCRIPTDIR}/routines/caltools.sh
##
##======================================================================
##----------------------------------------------------------------------
##    II. modify namelist
##----------------------------------------------------------------------
##======================================================================
##
#

if [ ${DATE_BEGIN_JOB} -eq ${DATE_BEGIN_EXP} ]; then
  rst="false"
else
  rst="true"
fi

sed -e "s/<yr1>/${YEAR_BEGIN_JOB}/g"   -e "s/<yr2>/${YEAR_END_JOB}/g"  \
    -e "s/<mo1>/${MONTH_BEGIN_JOB}/g"   -e "s/<mo2>/${MONTH_END_JOB}/g"  \
    -e "s/<dy1>/${DAY_BEGIN_JOB}/g"   -e "s/<dy2>/${DAY_END_JOB}/g"  \
    -e "s/<hr1>/0/g"   -e "s/<hr2>/24/g"  \
    -e "s/<rst>/$rst/g"              -e "s/<rst_int_h>/$(( ${TOTAL_JOB_DUR} * 24 ))/g"            \
    -e "s/<his_int_h>/${atm_his_h}/g"         -e "s/<his_nb_out>/${atm_his_frames}/g"    \
    -e "s/<xtrm_int_m>/${atm_diag_int_m}/g"   -e "s/<xtrm_nb_out>/${atm_diag_frames}/g"  \
    -e "s/<nproc_x>/${atm_nprocX}/g"            -e "s/<nproc_y>/${atm_nprocY}/g"             \
    -e "s/<niotaskpg>/${atm_niotaskpg}/g"       -e "s/<niogp>/${atm_niogp}/g"                \
    -e "s/time_step                           =.*/time_step                           =${TSP_ATM} ,/g"                    \
    $ATM_NAM_DIR/${atmnamelist} > ./namelist.input


