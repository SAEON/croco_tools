#!/bin/csh
########################################################
#  Define environment variables for XEON
########################################################
setenv OMP_SCHEDULE static
setenv OMP_NUM_THREADS 1
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
#           USER DEFINED RUN PARAMETERS
########################################################
set MODEL=roms
set SCRATCHDIR=`pwd`
set INPUTDIR=`pwd`
set MSSDIR=`pwd`
set MSSOUT=`pwd`
set CODFILE=roms
set AGRIF_FILE=AGRIF_FixedGrids.in
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
set NY_END=2
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

########################################################
#           USER DEFINED RUN PARAMETERS
########################################################
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
echo "Getting $CODFILE from $MSSDIR"
$CP -f $MSSDIR/$CODFILE $SCRATCHDIR
chmod u+x $CODFILE
$CP -f $MSSDIR/$AGRIF_FILE $SCRATCHDIR
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
    $CP -f $MSSDIR/${INIFILE}${ENDF} ${MODEL}_ini.nc${ENDF}
  else
    echo "Getting ${RSTFILE}${ENDF} from $MSSOUT"
    $CP -f $MSSOUT/${RSTFILE}${ENDF} $SCRATCHDIR
    $CP -f $MSSOUT/${RSTFILE}${ENDF} ${MODEL}_ini.nc${ENDF}
  endif
    echo "Getting ${MODEL}.in${ENDF} from $INPUTDIR"
    $CP -f $INPUTDIR/${MODEL}.in${ENDF} ${MODEL}.in${ENDF}
  @ LEVEL++
end
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
    $CODFILE  ${MODEL}.in > ${MODEL}_${TIME}.out
    date
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

      $CP -f ${MODEL}_rst.nc${ENDF} ${MODEL}_ini.nc${ENDF}
      $MV -f ${MODEL}_his.nc${ENDF} ${MODEL}_his_${TIME}.nc${ENDF}
      $MV -f ${MODEL}_rst.nc${ENDF} ${MODEL}_rst_${TIME}.nc${ENDF}
      $MV -f ${MODEL}_avg.nc${ENDF} ${MODEL}_avg_${TIME}.nc${ENDF}

      $MV -f ${MODEL}_dia.nc${ENDF} ${MODEL}_dia_${TIME}.nc${ENDF}
      $MV -f ${MODEL}_dia_avg.nc${ENDF} ${MODEL}_dia_avg_${TIME}.nc${ENDF}
      $MV -f ${MODEL}_diaM.nc${ENDF} ${MODEL}_diaM_${TIME}.nc${ENDF}
      $MV -f ${MODEL}_diaM_avg.nc${ENDF} ${MODEL}_diaM_avg_${TIME}.nc${ENDF}

      @ LEVEL++
    end
    @ NM++
  end
  @ NY++
end
#
#############################################################












