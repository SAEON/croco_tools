#!/bin/bash
#
#======================================================================
#
#        Script for running ROMS-WRF coupled simulations 
#                       with OASIS coupler
#
#======================================================================
#
export ROMS_FILES_DIR=$PWD/roms_files
export WRF_FILES_DIR=$PWD/wrf_files
export OASIS_FILES_DIR=$PWD/oasis_files
export WORKDIR=$PWD/outputs
export ROMS_EXE_DIR=$ROMS_FILES_DIR
export WRF_EXE_DIR=$HOME/WRF/WRFV3_coupled/run

NB_PROC_ROMS=2
NB_PROC_WRF=2
#
#=============== END USER CHANGES =====================================
#
# Clean Workdir
rm -Rf $WORKDIR/*

# Link executable
ln -sf $ROMS_EXE_DIR/roms $WORKDIR/roms3d
ln -sf $WRF_EXE_DIR/wrf.exe $WORKDIR/wrfexe

# Copy the roms input files
ln -sf ${ROMS_FILES_DIR}/roms_grd.nc* $WORKDIR
ln -sf ${ROMS_FILES_DIR}/roms_ini.nc* $WORKDIR
ln -sf ${ROMS_FILES_DIR}/roms_clm.nc $WORKDIR
ln -sf ${ROMS_FILES_DIR}/roms_frc.nc $WORKDIR
ln -sf ${ROMS_FILES_DIR}/roms.in* $WORKDIR

# For AGRIF
ln -sf ${ROMS_FILES_DIR}/AGRIF_FixedGrids.in $WORKDIR

# Copy the wrf input file
ln -sf ${WRF_FILES_DIR}/*_d01 $WORKDIR
ln -sf ${WRF_FILES_DIR}/*TBL $WORKDIR
ln -sf ${WRF_FILES_DIR}/*DATA $WORKDIR
ln -sf ${WRF_FILES_DIR}/*DBL $WORKDIR
ln -sf ${WRF_FILES_DIR}/namelist.input $WORKDIR

# Copy the oasis input files
ln -sf ${OASIS_FILES_DIR}/cf_name_table.txt $WORKDIR
ln -sf ${OASIS_FILES_DIR}/namcouple $WORKDIR/namcouple
ln -sf ${OASIS_FILES_DIR}/*.nc $WORKDIR/

## Execution
cd $WORKDIR
mpirun -np $NB_PROC_WRF ./wrfexe : -np $NB_PROC_ROMS ./roms3d
#mpirun -np $NB_PROC_WRF ./wrfexe : -np $NB_PROC_ROMS ./roms3d > run.log 2>&1
cd ../
#

