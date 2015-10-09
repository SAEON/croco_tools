#!/bin/bash
#
#======================================================================
#
#        OASIS namcouple generator for ROMS-WRF coupling
#
#======================================================================
#
functions_path=$HOME/OASIS/functions

export ROMS_FILES_DIR=$HOME/GIT/croco/OASIS/Run_benguela/roms_files
export WRF_FILES_DIR=$HOME/GIT/croco/OASIS/Run_benguela/wrf_files
export OASIS_FILES_DIR='./'

roms_grid_file=$ROMS_FILES_DIR/roms_grd.nc
wrf_grid_file=$WRF_FILES_DIR/geo_em.d01.nc
wrf_inp_file=$WRF_FILES_DIR/wrfinput_d01

myconfig='TOY'
mydate=`date +%Y%m%d`

max_domains_WRF=1
max_domains_ROMS=1
#
#=============== END USER CHANGES =====================================
#
. ${functions_path}/create_roms_grids_function.sh
. ${functions_path}/create_roms_masks_function.sh
. ${functions_path}/create_roms_corners_function.sh
. ${functions_path}/create_roms_areas_function.sh
. ${functions_path}/create_wrf_grids_function.sh
. ${functions_path}/create_wrf_masks_function.sh
. ${functions_path}/create_wrf_corners_function.sh
. ${functions_path}/create_wrf_areas_function.sh
. ${functions_path}/create_wrf_CPLMASK_function.sh
. ${functions_path}/duplicate_wrf_CPLMASK_function.sh
#
#--------------
#1. Grid names
#--------------
RRN0='rrn0' # Roms Rho Normal    0=Parent
RRP0='rrp0' # Roms Rho Processed 0=Parent
RUN0='run0' # Roms U   Normal    0=Parent
RUP0='rup0' # Roms U   Processed 0=Parent
RVN0='rvn0' # Roms V   Normal    0=Parent
RVP0='rvp0' # Roms V   Processed 0=Parent
#
WRN0='wrn0' # WRF Rho Normal    0=Parent
WRP0='wrp0' # WRF Rho Processed 0=Parent
#
#-----------------------------------------------
# 2. Processing ROMS Grids: Parent and Children
#-----------------------------------------------
DNroms=1
while [ $DNroms -le $max_domains_ROMS ]; do
  SNroms=`expr $DNroms \- 1`
  create_roms_grids_function $SNroms
  create_roms_corners_function $SNroms
  create_roms_masks_function $SNroms
  create_roms_areas_function $SNroms
  if [ $DNroms = 1 ]; then
     ncks -A corners_roms.nc grids_roms.nc
  else
     ncks -A corners_roms.nc.$SNroms grids_roms.nc.$SNroms
     ncks -A grids_roms.nc.$SNroms grids_roms.nc
     ncks -A masks_roms.nc.$SNroms masks_roms.nc
     ncks -A areas_roms.nc.$SNroms areas_roms.nc
  fi
  let DNroms=DNroms+1
done
#
#----------------------------------------------
# 3. Processing WRF Grids: Parent and Children
#----------------------------------------------
#
DNwrf=1
while [ $DNwrf -le $max_domains_WRF ]; do
  SNwrf=`expr $DNwrf \- 1`
  create_wrf_grids_function $SNwrf
  create_wrf_masks_function $SNwrf
  create_wrf_corners_function $SNwrf
  create_wrf_areas_function $SNwrf
  create_wrf_CPLMASK_function $SNwrf
  DNroms=2
  if [ $max_domains_ROMS -gt 1 ]; then
     duplicate_wrf_CPLMASK_function $SNwrf $max_domains_ROMS
  fi
  if [ $DNwrf = 1 ]; then
     ncks -A corners_wrf.nc grids_wrf.nc
  else
     ncks -A corners_wrf.nc.$SNwrf grids_wrf.nc.$SNwrf
     ncks -A grids_wrf.nc.$SNwrf grids_wrf.nc
     ncks -A masks_wrf.nc.$SNwrf masks_wrf.nc
     ncks -A areas_wrf.nc.$SNwrf areas_wrf.nc
  fi
  let DNwrf=DNwrf+1
done
#
#-----------------------------------
# 4. Merging ROMS and WRF  the files
#-----------------------------------
echo Merging all ROMS and WRF files
mv -f grids_roms.nc grids.nc
ncks -A grids_wrf.nc grids.nc
mv -f masks_roms.nc masks.nc
ncks -A masks_wrf.nc masks.nc
mv -f areas_roms.nc areas.nc
ncks -A areas_wrf.nc areas.nc
rm -f grids_*.nc* corners*.nc* masks_*.nc* areas_*.nc*

#
#----------------------------------------
# 5. Clean history and global Attributes
#----------------------------------------
ncatted -h -O -a ,global,d,, grids.nc
ncatted -h -a title,global,c,c,"${myconfig}" grids.nc
ncatted -h -a date,global,c,c,${mydate} grids.nc

ncatted -h -O -a ,global,d,, masks.nc
ncatted -h -a title,global,c,c,"${myconfig}" masks.nc
ncatted -h -a date,global,c,c,${mydate} masks.nc

ncatted -h -O -a ,global,d,, areas.nc
ncatted -h -a title,global,c,c,"${myconfig}" areas.nc
ncatted -h -a date,global,c,c,${mydate} areas.nc

echo Moving files
mv -f grids.nc areas.nc masks.nc CPLMASK* ${OASIS_FILES_DIR}
#
#----------------------------
# 5. Generate namcouple file
#----------------------------
#make_namcouple.sh
#
