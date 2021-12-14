#!/bin/bash -e

# --------------------------------------------------
# Script to extract wind components 
# from WRF output file(s)
# and create WW3 wind input file
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

# Directory where to find WRF wind files
dirin=$CWORK/outputs_wrf_frc
# Name or prefix of WRF netcdf files to be transformed for ww3
filewrf=wrfout_d01_2009-01-
# Names of u, v variables in WRF input file(s). NB: time variable XTIME has to be in WRF file too!
varnames=(U10 V10)
# Flag if concatenatenation is necessary
flag_concatenate=1

# Name of ww3 wind input file (declared in ww3_prnc.inp)
ww3windfile='WRF_wind_Y2009M1.nc'

# WW3 files input directory
dir_ww3_files=$WAV_FILES_DIR

#===============================================
#          END USER CHANGES
#===============================================

# Enter in the working directory
#===========================================
echo 'WW3 input directory is '$dir_ww3_files
echo ' '
cd $dir_ww3_files

# Extract or concatenate u, v, ant time variables
#===========================================
varlist=${varnames[0]},${varnames[1]},XTIME
if [ $flag_concatenate == 1 ] ; then
    filewrf="${dirin}/${filewrf}*"
    filegrid=`ls ${filewrf} | tail -1`
    ncrcat -O -v $varlist $filewrf $ww3windfile
else
    filewrf="${dirin}/${filewrf}"
    filegrid=$filewrf
    ncks -O -v $varlist $filewrf $ww3windfile
fi

# Rename u, v variables
#===========================================
if [ ${varnames[0]} != U10 ] ; then
  ncrename -v ${varnames[0]},U10 -v ${varnames[1]},V10 $ww3windfile 
fi
ncrename -d west_east,lon -d south_north,lat $ww3windfile

# Rename time and create correct time attributes
#===========================================
ncks -3 -O $ww3windfile $ww3windfile
ncrename -d Time,time -v XTIME,time $ww3windfile
ncks -4 -O $ww3windfile $ww3windfile
ncatted -O -a calendar,time,o,c,proleptic_gregorian $ww3windfile

# Create WRF 2D lon/lat file if necessary
#===========================================
if [ ! -f WRF_lon_lat.nc ] ; then
    ncks -v XLONG,XLAT $filegrid WRF_lon_lat.nc
    ncwa -O -a Time WRF_lon_lat.nc WRF_lon_lat.nc
    ncks -3 -O WRF_lon_lat.nc WRF_lon_lat.nc
    ncrename -v XLONG,lon -v XLAT,lat -d west_east,lon -d south_north,lat WRF_lon_lat.nc
    ncks -4 -O WRF_lon_lat.nc WRF_lon_lat.nc
fi

# Append WRF 2D lon/lat into ww3 wind file
#===========================================
ncks -A -v lon,lat WRF_lon_lat.nc $ww3windfile


