#!/bin/bash -e

# --------------------------------------------------
# Script to extract wind components 
# from CFSR files processed with Process_CFSR_files_for_CROCO.sh
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

# Directory where to find CFSR wind files
dirin=$CWORK/DATA/CFSR_Benguela_LR/
# Name of CFSR netcdf files to be transformed for ww3
fileu=U-component_of_wind_Y2005M1.nc
filev=V-component_of_wind_Y2005M1.nc

# date of origin ( days) of the CFSR data [ have to be compatible with the crocotools_param.m ]
startdate="2000-01-01 00:00:00"

# Flag if concatenatenation is necessary
flag_concatenate=0

# Name of ww3 wind input file (declared in ww3_prnc.inp)
ww3windfile='CFSR_wind_Y2005M01.nc'

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

# Extract or concatenate u, v variables
#===========================================
if [ $flag_concatenate == 1 ] ; then
    ncrcat -O $dirin/$fileu $ww3windfile
    ncrcat -A $dirin/$filev $ww3windfile
else
    cp -f $dirin/$fileu $ww3windfile
    ncks -A $dirin/$filev $ww3windfile
fi

# Rename u, v variables
#===========================================
ncrename -v U-component_of_wind,U10M -v V-component_of_wind,V10M $ww3windfile

# Add needed attribute : _FillValue
#===========================================
ncatted -O -a _FillValue,U10M,o,f,9999 $ww3windfile
ncatted -O -a _FillValue,V10M,o,f,9999 $ww3windfile

# Set the time attributes of the files
#===========================================
ncatted -O -a calendar,time,o,c,'proleptic_gregorian' $ww3windfile
ncatted -O -a units,time,m,c,"days since $startdate" $ww3windfile



