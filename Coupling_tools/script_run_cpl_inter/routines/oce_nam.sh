#!/bin/bash
set -ue
#set -vx
#
##======================================================================
##======================================================================
## This script automatically modify the CROCO "namelist"
##======================================================================
##======================================================================
#
#
##
##======================================================================
##----------------------------------------------------------------------
##    I. Calendar computations
##----------------------------------------------------------------------
##======================================================================
##
#
# some calendar tools...
#
#                           *** WARNING ***
#   To get back the functions defined in caltools.sh, we have to call 
#   it with ". caltools.sh" instruction. If we directly call "caltools.sh",
#   we will not have the functions back
#
. ${SCRIPTDIR}/routines/caltools.sh
#
##
##======================================================================
##----------------------------------------------------------------------
##    II. modify namelist
##----------------------------------------------------------------------
##======================================================================
##
#
for nn in $( seq 0 ${AGRIFZ} ) 
do
    if [ ${nn} -gt 0 ];    then
	namfile=croco.in.${nn}
	cp ${OCE_NAM_DIR}/croco.in.base.${nn} ${namfile}
        cp ${OCE_NAM_DIR}/AGRIF_FixedGrids.in ./
	SUBTIME=$( sed -n -e "$(( 2 * ${nn} )) p" AGRIF_FixedGrids.in | awk '{print $7 }' )
    else
	namfile=croco.in
	cp ${OCE_NAM_DIR}/croco.in.base ${namfile}
	SUBTIME=1
    fi
    TSP_OCE_2=$(( ${TSP_OCE} / ${SUBTIME} ))
#-------
## Number of time step per day
#-------
    OCE_NTSP_DAY=$(( 86400 / ${TSP_OCE_2} ))
    OCE_NTIMES=$(( ( ${JDAY_END_JOB} - ${JDAY_BEGIN_JOB} + 1 ) * ${OCE_NTSP_DAY}     ))
#
#-------
# change some namelist values
#-------
# Change in endding date for online interpolation

    mdy=$( valid_date $(( $MONTH_END_JOB + 1 )) $DAY_END_JOB $YEAR_END_JOB )
    end_Y=$( printf "%04d\n"  $( echo $mdy | cut -d " " -f 3) )
    end_M=$( printf "%01d\n"  $( echo $mdy | cut -d " " -f 1) )
#


sed -e "s/<ocentimes>/${OCE_NTIMES}/g" -e "s/<ocedt>/${TSP_OCE_2}/g"   -e "s/<ocendtfast>/${TSP_OCEF}/g" \
    -e "s/<oce_nrst>/${OCE_NTIMES}/g"   -e "s/<oce_nhis>/${oce_nhis}/g" -e "s/<oce_navg>/${oce_navg}/g"     \
    -e "s/<yr1>/${YEAR_BEGIN_JOB}/g"             -e "s/<mo1>/${MONTH_BEGIN_JOB}/g"           \
    -e "s/<yr2>/${end_Y}/g"             -e "s/<mo2>/${end_M}/g"           \
    ${namfile} > namelist.tmp

if [ ${USE_XIOS_OCE} -eq 1 ]; then
    sed -e "s/<title>/${OCE_OUTPUT_PREFIX}/g" \
    namelist.tmp > tmp$$
else
    sed -e "s/<title>/${CEXPER}/g" \
    namelist.tmp > tmp$$
fi
    mv tmp$$ namelist.tmp
    mv namelist.tmp ${namfile}
#
#cat namelist
#
done


exit






