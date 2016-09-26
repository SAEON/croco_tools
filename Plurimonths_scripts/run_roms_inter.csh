#!/bin/csh
#
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
#
unalias cp
unalias mv
limit coredumpsize unlimited
set CP=/bin/cp
set MV=/bin/mv
########################################################
#  Define files and run parameters
########################################################
#
set MODEL=roms
set SCRATCHDIR=`pwd`/SCRATCH
set INPUTDIR=`pwd`
set MSSDIR=`pwd`/ROMS_FILES
set MSSOUT=`pwd`/ROMS_FILES
set CODFILE=roms
set AGRIF_FILE=AGRIF_FixedGrids.in
#
set BULK_FILES=1
set FORCING_FILES=1
set CLIMATOLOGY_FILES=1
set BOUNDARY_FILES=0
#
# Atmospheric surface forcing dataset used for the bulk formula (NCEP)
#
set ATMOS_BULK=NCEP2
#
# Atmospheric surface forcing dataset used for the wind stress (NCEP, QSCAT)
#
set ATMOS_FRC=NCEP2
#
# Oceanic boundary and initial dataset (SODA, ECCO,...)
#
set OGCM=SODA
#
# Model time step [seconds]
#
set DT=3600
#
# number total of grid levels (1: No child grid)
#
set NLEVEL=1
#
set NY_START=2000
set NY_END=2000
set NM_START=1
set NM_END=3
#
# Number of year that are considered to be part of the spin-up (i.e. 365 days per year)
set NY_SPIN=0
#
#  Restart file - RSTFLAG=0 --> No Restart
#		  RSTFLAG=1 --> Restart
#
set RSTFLAG=0
#
#  Time Schedule  -  TIME_SCHED=0 --> yearly files
#                    TIME_SCHED=1 --> monthly files
#
set TIME_SCHED=1
#
########################################################
#
if ($TIME_SCHED == 0) then
  set NM_START=1979
  set NM_END=1979
endif
#
# netcdf file prefixes
#
set GRDFILE=${MODEL}_grd
set FRCFILE=${MODEL}_frc
set BLKFILE=${MODEL}_blk
set INIFILE=${MODEL}_ini
set CLMFILE=${MODEL}_clm
set BRYFILE=${MODEL}_bry
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
  set RSTFILE=${MODEL}_rst_${TIME}
endif
#
if ($TIME_SCHED == 0) then
  set TIME=Y${NY_START}
else
  set TIME=Y${NY_START}M${NM_START}
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
  echo "Getting ${GRDFILE}.nc${ENDF} from $MSSDIR"
  $CP -f $MSSDIR/${GRDFILE}.nc${ENDF} $SCRATCHDIR
  echo "Getting ${MODEL}_inter.in${ENDF} from $INPUTDIR"
  $CP -f $INPUTDIR/${MODEL}_inter.in${ENDF} $SCRATCHDIR
  if ($RSTFLAG == 0) then
    echo "Getting ${INIFILE}_${OGCM}_${TIME}.nc${ENDF} from $MSSDIR"
    $CP -f $MSSDIR/${INIFILE}_${OGCM}_${TIME}.nc${ENDF} $SCRATCHDIR
    $CP -f ${INIFILE}_${OGCM}_${TIME}.nc${ENDF} ${INIFILE}.nc${ENDF}
  else
    echo "Getting ${RSTFILE}.nc${ENDF} from $MSSOUT"
    $CP -f $MSSOUT/${RSTFILE}.nc${ENDF} $SCRATCHDIR
    $CP -f ${RSTFILE}.nc${ENDF} ${INIFILE}.nc${ENDF}
  endif
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
# Get forcing and clim for this time
#
    set LEVEL=0
    while ($LEVEL != $NLEVEL)
      if (${LEVEL} == 0) then
        set ENDF=
      else
        set ENDF=.${LEVEL}
      endif
      if (${FORCING_FILES} == 1) then
        echo "Getting ${FRCFILE}_${ATMOS_FRC}_${TIME}.nc${ENDF} from $MSSDIR"
        $CP -f $MSSDIR/${FRCFILE}_${ATMOS_FRC}_${TIME}.nc${ENDF} ${FRCFILE}.nc${ENDF}
      endif
      if (${BULK_FILES} == 1) then
        echo "Getting ${BLKFILE}_${ATMOS_BULK}_${TIME}.nc${ENDF} from $MSSDIR"
        $CP -f $MSSDIR/${BLKFILE}_${ATMOS_BULK}_${TIME}.nc${ENDF} ${BLKFILE}.nc${ENDF}
      endif
      @ LEVEL++
    end
#
# No child climatology or boundary files
#
    if (${CLIMATOLOGY_FILES} == 1) then
      echo "Getting ${CLMFILE}_${OGCM}_${TIME}.nc from $MSSDIR"
      $CP -f $MSSDIR/${CLMFILE}_${OGCM}_${TIME}.nc ${CLMFILE}.nc
    endif
    if (${BOUNDARY_FILES} == 1) then
      echo "Getting ${BRYFILE}_${OGCM}_${TIME}.nc from $MSSDIR"
      $CP -f $MSSDIR/${BRYFILE}_${OGCM}_${TIME}.nc ${BRYFILE}.nc
    endif
#
# Set the number of time steps for each month 
# (30 or 31 days + 28 or 29 days for february)
#
    set NUMTIMES=0
#
    if (${NM} == 1 || ${NM} == 3 || ${NM} == 5 || ${NM} == 7 || ${NM} == 8 || ${NM} == 10 || ${NM} == 12 ) then
      set NDAYS = 31
    else
      set NDAYS = 30
      if (${NM} == 2) then
        set NDAYS = 28
# February... check if it is a leap year

        set B4=0
        set B100=0
        set B400=0

        @ B4 = 4 * ( $NY / 4 )
        @ B100 = 100 * ( $NY / 100 )
        @ B400 = 400 * ( $NY / 400 )

	
        if ($NY == $B4 & ((!($NY == $B100))||($NY == $B400))) then
#
          set BSPIN=0
          @ BSPIN = 1 + $NY - ${NY_START}
          if ($BSPIN > $NY_SPIN) then
	     echo Leap Year - $NY $B4 $B100 $B400
             set NDAYS = 29
          else
#.........   SPINUP!!!! In case of spinup I cant have leap years.
	     echo year $NY should be a Leap Year     
	     echo 'BUT : Spinup case: no leap year'
             set NDAYS = 28
          endif
#
        else
	  echo Not a Leap Year - $NY $B4 $B100 $B400
          set NDAYS = 28	  		  
        endif
      endif
    endif
    @ NUMTIMES = $NDAYS * 24 * 3600
    @ NUMTIMES = $NUMTIMES / $DT
    echo "YEAR = $NY MONTH = $NM DAYS = $NDAYS DT = $DT NTIMES = $NUMTIMES"
# 
    echo "Writing in ${MODEL}_${TIME}_inter.in"
    set LEVEL=0
    while ($LEVEL != $NLEVEL)
      if (${LEVEL} == 0) then
        set ENDF=
      else
        set ENDF=.${LEVEL}
	@ NUMTIMES = 3 * $NUMTIMES
      endif
      echo "USING NUMTIMES = $NUMTIMES"
      sed -e 's/NUMTIMES/'$NUMTIMES'/' -e 's/NYONLINE/'$NY'/' -e 's/NMONLINE/'$NM'/' < ${MODEL}_inter.in${ENDF} > ${MODEL}_${TIME}_inter.in${ENDF}
      @ LEVEL++
    end
#
#  COMPUTE
#
    date
    ./$CODFILE  ${MODEL}_${TIME}_inter.in > ${MODEL}_${TIME}.out
    date
#
# Test if the month has finised properly
    echo "Test ${MODEL}_${TIME}.out"
    tail -2 ${MODEL}_${TIME}.out | grep DONE >& /dev/null
    if ($status == 0) then
      echo "All good"
      echo "XXXX${MYTEST}XXXX"
    else
      echo
      echo "Warning: month not finished properly"
      echo
      tail -20 ${MODEL}_${TIME}.out
      echo
      echo "Month ${TIME} did not work after ${NRUN} attempts"
      echo
      exit 1
    end
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
      $CP -f ${MODEL}_rst.nc${ENDF} ${INIFILE}.nc${ENDF}
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












