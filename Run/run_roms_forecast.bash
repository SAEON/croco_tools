#!/bin/bash
#
#---------------------------------------------
#  Define environment variables for XEON
#---------------------------------------------
#
# # Environment for Crontab
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi
source ~/.bashrc

export IA32ROOT=/opt/intel/Compiler/11.1/056/bin/intel64
source $IA32ROOT/ifortvars_intel64.sh
export F_UFMTENDIAN=big
ulimit -s 131072
export KMP_STACKSIZE=50m
export OMP_NUM_THREADS=4
#
export HOME=/home/gcambon
export TOOLSDIR=$HOME/Roms_tools/Forecast_tools
export RUNDIR=${HOME}/Roms_tools/Run
export MATLAB=/usr/local/bin/matlab
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/loaddap-3.5.2/lib

unalias cp
unalias mv
export CP=/bin/cp
export MV=/bin/mv
export LN=/bin/ln
#-----------------------------------------------
#  Define files and run parameters
#-----------------------------------------------
echo Start the forecast 
date
#
# Get forcing Files from DODS SERVER and process them for ROMS
# PRE_PROCESS=1 ==> do the work (0 otherwise)
export PRE_PROCESS=0
#
# Perform Iterations for convergence of ROMS and OGCM
# ITERATION=1 ==> do several hindcasts using nudging
export ITERATION=0
#
export SCRATCHDIR=${RUNDIR}/SCRATCH
export INPUTDIR=${RUNDIR}
export MSSDIR=${RUNDIR}/ROMS_FILES
export MSSOUT=${RUNDIR}/FORECAST
#
export MODEL=roms
export CODFILE=roms
#
export GRDFILE=${MODEL}_grd.nc
export INIFILE=${MODEL}_ini.nc
export RSTFILE=${MODEL}_rst.nc
export AVGFILE=${MODEL}_avg.nc
export HISFILE=${MODEL}_his.nc
export BLKFILE=${MODEL}_blk_GFS_0.nc
export FRCFILE=${MODEL}_frc_GFS_0.nc
export BRYFILE=${MODEL}_bry_mercator_0.nc
export CLMFILE=${MODEL}_clm_mercator_0.nc

#
####################################################################
#
# Go the Input directory 
# 
cd $INPUTDIR
#
# Cleaning
#

rm -f $MSSDIR/${MODEL}_blk_* $MSSDIR/${MODEL}_frc_* $MSSDIR/${MODEL}_bry_*
rm -f $MSSDIR/${MODEL}_clm_* $MSSDIR/${MODEL}_ini_*
rm -f $MSSOUT/${MODEL}_his_* $MSSOUT/${MODEL}_avg_* $MSSOUT/${MODEL}_rst_*

#
# Compute lateral boundaries from Mercator and surface forcing from GFS
#
if [ $PRE_PROCESS = 1 ] ; then
  echo "Processing boundary and forcing files"
  $MATLAB  -batch -nodisplay -nojvm < make_forecast.m > matlab_forecast.out
  echo "Getting $BLKFILE from $MSSDIR"
  $CP -f  $MSSDIR/$BLKFILE $SCRATCHDIR
  echo "Getting $FRCFILE from $MSSDIR"
  $CP -f  $MSSDIR/$FRCFILE $SCRATCHDIR
  echo "Getting $BRYFILE from $MSSDIR"
  $CP -f  $MSSDIR/$BRYFILE $SCRATCHDIR
  echo "Getting $CLMFILE from $MSSDIR"
  $CP -f  $MSSDIR/$CLMFILE $SCRATCHDIR
fi
#
# Get the initial conditions from the previous hindcast run
#
echo "Getting $INIFILE from $MSSOUT"
$CP -f  $MSSOUT/$INIFILE $SCRATCHDIR
#
# Getting the static files
#
echo "Getting $CODFILE from $INPUTDIR"
$CP -f $INPUTDIR/$CODFILE $SCRATCHDIR
chmod u+x $CODFILE
echo "Getting ${GRDFILE} from $MSSDIR"
$CP -f $MSSDIR/${GRDFILE} $SCRATCHDIR
echo "Getting ${MODEL}_hindcast.in from $INPUTDIR"
$CP -f $INPUTDIR/${MODEL}_hindcast.in $SCRATCHDIR
echo "Getting ${MODEL}_forecast.in from $INPUTDIR"
$CP -f $INPUTDIR/${MODEL}_forecast.in $SCRATCHDIR
#
#  Change directory
#
cd $SCRATCHDIR
#
#  COMPUTE 1: Hindcast run
#
echo 4 day hindcast run  
# 1 days in case ECCO ogcm and 4 days in case Mercator ogcm , 
# Don't forget to modify the NRPFRST and NNREC 
# in roms_hindcast.in and roms_forecast.in

date
./$CODFILE ${MODEL}_hindcast.in > ${MODEL}_hindcast_`date -Idate`.out

if [ $ITERATION = 1 ] ; then
echo 'ITERATION ITERATION'
$LN -sf $TOOLSDIR/iteration.m iteration.m
$LN -sf $RUNDIR/start.m start.m
$LN -sf $RUNDIR/romstools_param.m romstools_param.m
$MATLAB  -nodisplay -nojvm < iteration.m > iteration.out
rm -f iteration.m start.m romstools_param.m
fi
#
date
#
# Get the initial file for the forecast run
#
$CP -f $SCRATCHDIR/$RSTFILE ${SCRATCHDIR}/$INIFILE
#
# Store the initial file for the next hindcast run
#
$CP -f $SCRATCHDIR/$RSTFILE ${MSSOUT}/$INIFILE
#
# Store the output hindcast files
#
$MV $SCRATCHDIR/$HISFILE ${MSSOUT}/${MODEL}_his_hindcast_`date -Idate`.nc
$MV $SCRATCHDIR/$AVGFILE ${MSSOUT}/${MODEL}_avg_hindcast_`date -Idate`.nc
$MV $SCRATCHDIR/$RSTFILE ${MSSOUT}/${MODEL}_rst_hindcast_`date -Idate`.nc
#
#  COMPUTE 2: Forecast run
#
echo 5 days forecast run 
date
./$CODFILE ${MODEL}_forecast.in > ${MODEL}_forecast_`date -Idate`.out
date

#
# Store the output forecast files
#
$CP $SCRATCHDIR/$HISFILE ${MSSOUT}/${MODEL}_his_forecast_`date -Idate`.nc
$CP $SCRATCHDIR/$AVGFILE ${MSSOUT}/${MODEL}_avg_forecast_`date -Idate`.nc
#$CP $SCRATCHDIR/$AVGFILE /mnt/ftp_out/ROMSAGRIF_FORECAST/latest_forecast.nc
#
#  Analysis for the forecast run...
#
cd $INPUTDIR

$LN -sf $TOOLSDIR/plot_forecast_roms.m plot_forecast_roms.m
$LN -sf $TOOLSDIR/plot_quick_forecast.m plot_quick_forecast.m
# Production plot
$MATLAB  -batch -nodisplay -nojvm < plot_forecast_roms.m >  plot_forecast_roms.out

# Quick plot
#$MATLAB  -batch -nodisplay -nojvm < plot_quick_forecast.m >  plot_quick_forecast.out
#
rm -f plot_forecast_roms.m plot_quick_forecast.m


echo Forecast finished
date



