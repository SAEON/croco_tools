#!/bin/bash
#set -u


export CALTYPE=greg


##------------------------------------------------------------------------------
## Some calendar tools...
##------------------------------------------------------------------------------

function valid_date 
{
jd=$( ${SCRIPTDIR}/routines/julday.sh $1 $2 $3 $CALTYPE )
${SCRIPTDIR}/routines/caldat.sh $jd $CALTYPE 
}

function makedate
{ 
jd=$( ${SCRIPTDIR}/routines/julday.sh $1 $2 $3 $CALTYPE )
mdy=$( ${SCRIPTDIR}/routines/caldat.sh $jd $CALTYPE )
m=$( echo $mdy | cut -d " " -f 1 )
d=$( echo $mdy | cut -d " " -f 2 )
y=$( echo $mdy | cut -d " " -f 3 )
echo $( printf "%04d\n" $y)$( printf "%02d\n" $m)$( printf "%02d\n" $d) 
}


function sec2hour
{
secs=$1
h=$(( $secs / 3600 ))
m=$(( ( $secs / 60 ) % 60 ))
s=$(( $secs % 60 ))

echo $( printf "%02d:%02d:%02d\n" $h $m $s )
}

##------------------------------------------------------------------------------
## Date of the beginning of the experiment (in $CALTYPE calendar):
##------------------------------------------------------------------------------
export DATE_BEGIN_EXP=$( makedate $MONTH_BEGIN_EXP $DAY_BEGIN_EXP $YEAR_BEGIN_EXP )

##------------------------------------------------------------------------------
## Date of the beginning of the experiment (in julian calendar (in days)):
##------------------------------------------------------------------------------
export JDAY_BEGIN_EXP=$( ${SCRIPTDIR}/routines/julday.sh ${MONTH_BEGIN_EXP} ${DAY_BEGIN_EXP} ${YEAR_BEGIN_EXP} $CALTYPE )

##------------------------------------------------------------------------------
# Date of the end of the experiment (in $CALTYPE calendar):
##------------------------------------------------------------------------------
mdy=$( valid_date $(( $MONTH_BEGIN_EXP + $EXP_DUR_MTH )) $(( $DAY_BEGIN_EXP + $EXP_DUR_DAY - 1 )) $YEAR_BEGIN_EXP )
export MONTH_END_EXP=$( echo $mdy | cut -d " " -f 1 )
export DAY_END_EXP=$(   echo $mdy | cut -d " " -f 2 )
export YEAR_END_EXP=$(  echo $mdy | cut -d " " -f 3 )
#
export DATE_END_EXP=$( makedate $MONTH_END_EXP $DAY_END_EXP $YEAR_END_EXP )
#
[ $DATE_END_EXP -lt $DATE_BEGIN_EXP ] && echo "ERROR: DATE_END_EXP ($DATE_END_EXP) must be larger than DATE_BEGIN_EXP ($DATE_BEGIN_EXP)... We stop..." && exit

##------------------------------------------------------------------------------
# Date of the beginning of the job (in $CALTYPE calendar):
##------------------------------------------------------------------------------
export DATE_BEGIN_JOB=$( makedate $MONTH_BEGIN_JOB $DAY_BEGIN_JOB $YEAR_BEGIN_JOB )
[ $DATE_BEGIN_JOB -lt $DATE_BEGIN_EXP ] && echo "ERROR: DATE_BEGIN_JOB ($DATE_BEGIN_JOB) must be larger than DATE_BEGIN_EXP ($DATE_BEGIN_EXP)... We stop..." && exit
[ $DATE_BEGIN_JOB -gt $DATE_END_EXP ] && echo "ERROR: DATE_BEGIN_JOB ($DATE_BEGIN_JOB) must be smaller than DATE_END_EXP ($DATE_END_EXP)... We stop..." && exit

##------------------------------------------------------------------------------
## julian date of the beginning of the job
##------------------------------------------------------------------------------
export JDAY_BEGIN_JOB=$( ${SCRIPTDIR}/routines/julday.sh ${MONTH_BEGIN_JOB} ${DAY_BEGIN_JOB} ${YEAR_BEGIN_JOB} $CALTYPE )

##------------------------------------------------------------------------------
# estimation of the job duration and the end of the job in agreement with 
# the outputs frequency...
##------------------------------------------------------------------------------
#
## year month day of the end of job
mdy=$( valid_date $(( $MONTH_BEGIN_JOB + $JOB_DUR_MTH )) $(( $DAY_BEGIN_JOB + $JOB_DUR_DAY - 1 )) $YEAR_BEGIN_JOB )
export MONTH_END_JOB=$( echo $mdy | cut -d " " -f 1 )
export DAY_END_JOB=$(   echo $mdy | cut -d " " -f 2 )
export YEAR_END_JOB=$(  echo $mdy | cut -d " " -f 3 )
# julian date of the end of the job
JDAY_END_JOB=$( ${SCRIPTDIR}/routines/julday.sh ${MONTH_END_JOB} ${DAY_END_JOB} ${YEAR_END_JOB} $CALTYPE )
# total job duration
export TOTAL_JOB_DUR=$(( $JDAY_END_JOB - JDAY_BEGIN_JOB + 1 ))

##------------------------------------------------------------------------------
# date of the end of the job
##------------------------------------------------------------------------------
export DATE_END_JOB=$( makedate $MONTH_END_JOB $DAY_END_JOB $YEAR_END_JOB )
if [ $DATE_END_JOB -lt $DATE_BEGIN_JOB ] 
    then
    echo "ERROR: DATE_END_JOB ($DATE_END_JOB) must be larger than DATE_BEGIN_JOB ($DATE_BEGIN_JOB)... We stop..." 
    exit
fi
if [ $DATE_END_JOB -gt $DATE_END_EXP ] 
    then 
    echo "ERROR: DATE_END_JOB ($DATE_END_JOB) must be smaller than DATE_END_EXP ($DATE_END_EXP)... We stop..."
    exit
fi

##------------------------------------------------------------------------------
# define dates for next job...
##------------------------------------------------------------------------------
    JDAY_BEGIN_JOBp1=$(( ${JDAY_END_JOB} + 1 ))
    mdy=$( ${SCRIPTDIR}/routines/caldat.sh ${JDAY_BEGIN_JOBp1} ${CALTYPE} )
    MONTH_BEGIN_JOBp1=$( echo ${mdy} | cut -d " " -f 1 )
    DAY_BEGIN_JOBp1=$(   echo ${mdy} | cut -d " " -f 2 )
    YEAR_BEGIN_JOBp1=$(  echo ${mdy} | cut -d " " -f 3 )


