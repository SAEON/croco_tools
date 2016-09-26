#!/bin/csh
########################################################
#  Define environment variables for XEON
########################################################
setenv OMP_SCHEDULE static
setenv OMP_NUM_THREADS 2
setenv OMP_DYNAMIC false
setenv OMP_NESTED false
setenv KMP_LIBRARY throughput
setenv KMP_STACKSIZE 2m
setenv KMP_DUPLICATE_LIB_OK TRUE
unalias cp
unalias mv
limit coredumpsize unlimited
set CP=/bin/cp
set MV=/bin/mv
########################################################
#  Define files and run parameters
########################################################
set MODEL=roms
set SCRATCHDIR=`pwd`/SCRATCH
set INPUTDIR=`pwd`
set MSSDIR=`pwd`/ROMS_FILES
set MSSOUT=`pwd`/ROMS_FILES
set CODFILE=roms
set AGRIF_FILE=AGRIF_FixedGrids.in
#
# Model time step [seconds]
#
set DT=3600
#
# Number of days per month
#
set NDAYS = 30
#
# number total of grid levels
#
set NLEVEL=1
#
#  Time Schedule  -  TIME_SCHED=0 --> yearly files
#                    TIME_SCHED=1 --> monthly files
#
set TIME_SCHED=1
#
set NY_START=1
set NY_END=10
set NM_START=1
set NM_END=12
#
if ($TIME_SCHED == 0) then
  set NM_START=1
  set NM_END=1
endif
#
# coarse netcdf file names
#
set GRDFILE=${MODEL}_grd.nc
set FORFILE=${MODEL}_frc.nc
set CLMFILE=${MODEL}_clm.nc
set INIFILE=${MODEL}_ini.nc
#
#  Restart file - RSTFLAG=0 --> No Restart
#		  RSTFLAG=1 --> Restart
#
if ($NY_START == 1 & $NM_START == 1) then
  set RSTFLAG=0
else
  set RSTFLAG=1
endif
#
if ($RSTFLAG != 0) then
  set NY=$NY_START
  set NM=$NM_START
  if ($TIME_SCHED == 0) then
    @ NY--
    set TIME=Y${NY}
  else
    @ NM--
    if ($NM == 0) then
      set NM=12
      @ NY--
    endif
    set TIME=Y${NY}M${NM}
  endif
  set RSTFILE=${MODEL}_rst_${TIME}.nc
endif
#
# Get the code
#
cd $SCRATCHDIR
echo "Getting $CODFILE from $INPUTDIR"
$CP -f $INPUTDIR/$CODFILE $SCRATCHDIR
chmod u+x $CODFILE
echo "Getting $AGRIF_FILE from $INPUTDIR"
$CP -f $INPUTDIR/$AGRIF_FILE $SCRATCHDIR
#
# Get the netcdf files
#
set LEVEL=0
while ($LEVEL != $NLEVEL)
  if (${LEVEL} == 0) then
    set ENDF=
  else
    set ENDF=.${LEVEL}
  endif
  echo "Getting ${GRDFILE}${ENDF} from $MSSDIR"
  $CP -f $MSSDIR/${GRDFILE}${ENDF} $SCRATCHDIR
  echo "Getting ${FORFILE}${ENDF} from $MSSDIR"
  $CP -f $MSSDIR/${FORFILE}${ENDF} $SCRATCHDIR
  echo "Getting ${CLMFILE}${ENDF} from $MSSDIR"
  $CP -f $MSSDIR/${CLMFILE}${ENDF} $SCRATCHDIR
  if ($RSTFLAG == 0) then
    echo "Getting ${INIFILE}${ENDF} from $MSSDIR"
    $CP -f $MSSDIR/${INIFILE}${ENDF} $SCRATCHDIR
  else
    echo "Getting ${RSTFILE}${ENDF} from $MSSOUT"
    $CP -f $MSSOUT/${RSTFILE}${ENDF} $SCRATCHDIR
    $CP -f ${RSTFILE}${ENDF} ${MODEL}_ini.nc${ENDF}
  endif
  echo "Getting ${MODEL}_inter.in${ENDF} from $INPUTDIR"
  $CP -f $INPUTDIR/${MODEL}_inter.in${ENDF} ${MODEL}_inter.in${ENDF}

  @ LEVEL++
end
#
# Put the number of time steps in the .in files
#
set NUMTIMES=0
@ NUMTIMES = $NDAYS * 24 * 3600
@ NUMTIMES = $NUMTIMES / $DT
echo "Writing in ${MODEL}_inter.in"
set LEVEL=0
while ($LEVEL != $NLEVEL)
  if (${LEVEL} == 0) then
    set ENDF=
  else
    set ENDF=.${LEVEL}
    @ NUMTIMES = 3 * $NUMTIMES
  endif
  echo "USING NUMTIMES = $NUMTIMES"
  sed 's/NUMTIMES/'$NUMTIMES'/' < ${MODEL}_inter.in${ENDF} > ${MODEL}.in${ENDF}
  @ LEVEL++
end
#
###########################################################
#  Compute
###########################################################
#
@ NY_END++
@ NM_END++
set NY=$NY_START
while ($NY != $NY_END)
  if ($NY == $NY_START) then
    set NM=$NM_START
  else 
    set NM=1
  endif
  set MY_YEAR=$NY
  @ MY_YEAR++
  if ($MY_YEAR == $NY_END) then
    set MONTH_END=$NM_END
  else 
    set MONTH_END=13
  endif
  if ($TIME_SCHED == 0) then
    set MONTH_END=2
  endif
  while ($NM != $MONTH_END)
    if ($TIME_SCHED == 0) then
      set TIME=Y${NY}
      echo "Computing YEAR $NY"
    else
      set TIME=Y${NY}M${NM}
      echo "Computing YEAR $NY MONTH $NM"
    endif
#
#  COMPUTE
#
    date
    ./$CODFILE  ${MODEL}.in > ${MODEL}_${TIME}.out
    date
#
# Test if the month has finised properly
    tail -2 ${MODEL}_${TIME}.out | grep DONE >& /dev/null
    if ($status != 0) then
        echo
        echo "Month ${TIME} did not work"
        echo
        exit 1
    endif
#
#  Archive
#
    set LEVEL=0
    while ($LEVEL != $NLEVEL)
      if (${LEVEL} == 0) then
        set ENDF=
      else
        set ENDF=.${LEVEL}
      endif
      $CP -f ${MODEL}_rst.nc${ENDF} ${INIFILE}${ENDF}
      $MV -f ${MODEL}_his.nc${ENDF} ${MODEL}_his_${TIME}.nc${ENDF}
      $MV -f ${MODEL}_rst.nc${ENDF} ${MODEL}_rst_${TIME}.nc${ENDF}
      $MV -f ${MODEL}_avg.nc${ENDF} ${MODEL}_avg_${TIME}.nc${ENDF}
      @ LEVEL++
    end
    @ NM++
  end
  @ NY++
end
#
#############################################################












