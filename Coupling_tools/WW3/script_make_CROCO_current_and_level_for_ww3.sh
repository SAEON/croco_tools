#!/bin/bash -e

# --------------------------------------------------
# Script to extract current and water level 
# from CROCO output file(s)
# and create WW3 current and water level input files
# --------------------------------------------------
#
# Further Information:   
# http://www.croco-ocean.org
#  
# This file is part of CROCOTOOLS
#
# CROCOTOOLS is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2 of the License,
# or (at your option) any later version.
#
# CROCOTOOLS is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA
#
# Copyright (c) 2018 S. Jullien
# swen.jullien@ifremer.fr
# --------------------------------------------------

#===============================================
#          USER CHANGES
#===============================================
# source run_env where paths to the config. are defined
source ../../myenv_mypath.sh

# Directory where to find CROCO files
dirin=$CWORK/outputs_croco_frc_clim
# Name of CROCO netcdf files to be transformed for ww3
file=croco_avg.nc
# Flag if concatenatenation is necessary
flag_concatenate=0

# Name of ww3 current and level input files (declared in ww3_prnc.inp)
ww3curfile='CROCO_current_Y2009M1.nc'
ww3levfile='CROCO_level_Y2009M1.nc'

# date of CROCO simulation start
crocostartdate='2009-01-01 00:00:00'

# number of CROCO rho vertical levels
Ns_rho=32

# WW3 files input directory
dir_ww3_files=$WAV_FILES_DIR

# directory where UV2T.sh function is available
dirsh=$PWD

#===============================================
#          END USER CHANGES
#===============================================

# Enter in the working directory
#===========================================
echo 'WW3 input directory is '$dir_ww3_files
echo ' '
cd $dir_ww3_files

# Extract or concatenate u, v variables
#===========================================
if [ $flag_concatenate == 1 ] ; then
    ncrcat -F -O -d s_rho,$Ns_rho -v u,v,lon_rho,lat_rho,time $dirin/$file $ww3curfile
    ncrcat -F -O -v zeta,lon_rho,lat_rho,time $dirin/$file $ww3levfile
else
    ncks -F -O -d s_rho,$Ns_rho -v u,v,lon_rho,lat_rho,time $dirin/$file $ww3curfile
    ncks -O -v zeta,lon_rho,lat_rho,time $dirin/$file $ww3levfile
fi

# Put u, v current variables on grid T
#===========================================
# first remove unused vertical dimension
ncwa -O -a s_rho $ww3curfile $ww3curfile
$dirsh/UV2T.sh $ww3curfile u lon_rho,lat_rho,time xi_u xi_rho time
$dirsh/UV2T.sh $ww3curfile v lon_rho,lat_rho,time eta_v eta_rho time
mv -f $(basename $ww3curfile '.nc')_u_gridT.nc ${ww3curfile}
ncks -A -v v $(basename $ww3curfile '.nc')_v_gridT.nc ${ww3curfile}
rm -f $(basename $ww3curfile '.nc')_v_gridT.nc

# Rename variables
#===========================================
ncrename -v u,ucur -v v,vcur -d xi_rho,lon -d eta_rho,lat -v xi_rho,lon -v eta_rho,lat $ww3curfile
ncrename -v zeta,level -d xi_rho,lon -d eta_rho,lat -v xi_rho,lon -v eta_rho,lat $ww3levfile

# Set the time attributes of the files
#===========================================
ncatted -O -a calendar,time,o,c,'proleptic_gregorian' ${ww3curfile}
ncatted -O -a units,time,o,c,"seconds since $crocostartdate" ${ww3curfile} 

ncatted -O -a calendar,time,o,c,'proleptic_gregorian' ${ww3levfile}
ncatted -O -a units,time,o,c,"seconds since $crocostartdate" ${ww3levfile}

