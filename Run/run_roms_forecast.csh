#!/bin/csh
########################################################
#  Define environment variables for XEON
########################################################
#
set KMP_STACKSIZE=500000000
limit stacksize unlimited
#
set HOME=/home/ppenven/
set RUNDIR=${HOME}Roms_tools/Run
set PATH=${RUNDIR}:/opt/intel/fce/9.0/bin:/opt/intel/fce/9.0/bin:/usr/kerberos/bin:/usr/local/bin:/usr/bin:/bin:/usr/X11R6/bin:.:/home/ppenven/bin:.
set LD_LIBRARY_PATH=/usr/local/lib
set MATLAB=/opt/matlab_2006/bin/matlab
#
#
unalias cp
unalias mv
unalias rm
set CP=/bin/cp
set MV=/bin/mv
set RM=/bin/rm
#
########################################################
#  Define files and run parameters
########################################################
#
echo Start the forecast 
date
#
set SCRATCHDIR=${RUNDIR}/SCRATCH/
set INPUTDIR=${RUNDIR}
set MSSDIR=${RUNDIR}/ROMS_FILES/
set MSSOUT=${RUNDIR}/FORECAST/
#
set MODEL=roms
set CODFILE=roms
#
set GRDFILE=${MODEL}_grd.nc
set INIFILE_FCT=${MODEL}_ini_fct.nc
set INIFILE_HCT=${MODEL}_ini_hct.nc
set INIFILE_OGCM_HCT=${MODEL}_ini_ECCO_hct.nc
set RSTFILE=${MODEL}_rst.nc
set AVGFILE=${MODEL}_avg.nc
set HISFILE=${MODEL}_his.nc
set BLKFILE=${MODEL}_blk_GFS_0.nc
set FRCFILE=${MODEL}_frc_GFS_0.nc
set BRYFILE=${MODEL}_bry_ECCO_0.nc
set STAFILE=${MODEL}_sta.nc
#
# Length of the hindcast run 
# (normally 1... but could be longer to recover from a crash.)
#
set NDAY_HINDCAST=1
#
# Go the Input directory 
# 
cd $INPUTDIR
#
# Get the initial conditions from the previous hindcast run
#
#echo "Getting $INIFILE from $MSSOUT"
#$CP -f  $MSSOUT/$INIFILE $SCRATCHDIR
#
# Getting the file which don't change with time
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
echo "Getting ${MODEL}_stations.in from $INPUTDIR"
$CP -f $INPUTDIR/${MODEL}_stations.in $SCRATCHDIR
#
# Compute lateral boundaries (+initial cond) from ECCO and surface forcing from GFS
#
echo "Processing boundary and forcing files"
$MATLAB  -batch -nodisplay -nojvm < make_forecast.m > matlab_forecast.out
echo "Getting $BLKFILE from $MSSDIR"
$CP -f  $MSSDIR/$BLKFILE $SCRATCHDIR
echo "Getting $FRCFILE from $MSSDIR"
$CP -f  $MSSDIR/$FRCFILE $SCRATCHDIR
echo "Getting $BRYFILE from $MSSDIR"
$CP -f  $MSSDIR/$BRYFILE $SCRATCHDIR
#
#  Change directory
#
cd $SCRATCHDIR
#
# Get the initial file for the hindcast run
#
# 1: copy from the OGCM data
#
echo "Getting $INIFILE_OGCM_HCT from $MSSDIR"
$CP -f ${MSSDIR}/$INIFILE_OGCM_HCT ${SCRATCHDIR}/$INIFILE_HCT
#
# 2: overwrite with the initial file created during the 
#    previous hindcast run
#
echo "Overwrite with $INIFILE_HCT from $MSSOUT"
$CP -f ${MSSOUT}/$INIFILE_HCT ${SCRATCHDIR}/$INIFILE_HCT
#
#  COMPUTE 1: Hindcast run
#
echo ${NDAY_HINDCAST} day hindcast run
date
$SCRATCHDIR/$CODFILE $SCRATCHDIR/${MODEL}_hindcast.in > $SCRATCHDIR/${MODEL}_hindcast_`date -Idate`.out
date
#
# Get the initial file for the forecast run
#
$CP $SCRATCHDIR/$RSTFILE ${SCRATCHDIR}/$INIFILE_FCT
#
# Store the initial file for the next hindcast run
#
$RM ${MSSOUT}/$INIFILE_HCT
$CP $SCRATCHDIR/$RSTFILE ${MSSOUT}/$INIFILE_HCT
#
# Store the output hindcast files
#
$MV $SCRATCHDIR/$HISFILE ${MSSOUT}/${MODEL}_his_hindcast_`date -Idate`.nc
$MV $SCRATCHDIR/$AVGFILE ${MSSOUT}/${MODEL}_avg_hindcast_`date -Idate`.nc
$MV $SCRATCHDIR/$RSTFILE ${MSSOUT}/${MODEL}_rst_hindcast_`date -Idate`.nc
$CP -f $SCRATCHDIR/${MODEL}_sta_hindcast.nc ${MSSOUT}/${MODEL}_sta_hindcast_`date -Idate`.nc
#
#  COMPUTE 2: Forecast run
#
echo 8 days forecast run 
date
$SCRATCHDIR/$CODFILE $SCRATCHDIR/${MODEL}_forecast.in > $SCRATCHDIR/${MODEL}_forecast_`date -Idate`.out
date
#
# Store the output forecast files
#
$CP $SCRATCHDIR/$HISFILE ${MSSOUT}/${MODEL}_his_forecast_`date -Idate`.nc
$CP $SCRATCHDIR/$AVGFILE ${MSSOUT}/${MODEL}_avg_forecast_`date -Idate`.nc
$CP $SCRATCHDIR/${MODEL}_sta_forecast.nc ${MSSOUT}/${MODEL}_sta_forecast_`date -Idate`.nc
#
#  Analysis for the forecast run...
#
cd $INPUTDIR
$MATLAB  -batch -nodisplay -nojvm < forecast_analysis.m >  forecast_analysis.out
echo Forecast finished
date







