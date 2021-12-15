#!/bin/bash

# --------------------------------------------------
# Function to change a field from grid U or V to grid T
# Usage: ./UV2T.sh filein var lonlattime dimUV dimRHO dimT
#        filein is the netcdf input file
#        var is the field to regrid
#        lonlattime are the names of lon, lat and time fields separated by a coma
#        dimUV is the name of the X or Y dimension of field var on grid U or V, respectively
#        dimRHO is the name of the X or Y dimension on T points
#        dimT is the name of the time dimension     
#
# Output is a netcdf file named as filein with _gridT suffix containing var, lon, lat, time on the T grid
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

filein=$1
var=$2
lonlattime=$3
dimUV=$4
dimRHO=$5
dimT=$6

dirin=$(dirname "$filein")
fileout="$dirin/$(basename $filein '.nc')_${var}_gridT.nc"

# read the size of dimUV and dimT
N=`ncdump -h $filein | grep "$dimUV = " | cut -d '=' -f2 | cut -d ';' -f1`
Nt=`ncdump -h $filein | grep "$dimT = " | cut -d '=' -f2 | cut -d ';' -f1`
# suppress blanks in N and Nt
N=${N// /}
Nt=${Nt// /}
# compute N-1
Nm1=$((${N}-1))

# extract var from indices 1 to end-1 and rename
ncks -F -v $var -d ${dimUV},1,$Nm1 $filein ${filein}.tmpgridT
ncrename -v $var,${var}_1 ${filein}.tmpgridT
# extract var from indices 2 to end 
ncks -F -A -v $var -d ${dimUV},2,$N $filein ${filein}.tmpgridT
# compute var at point T (from indices 2 to end-1, considering that the grid starts with a T point)
ncap2 -O -v -s "${var}=0.5*(${var}+${var}_1)" ${filein}.tmpgridT ${fileout}

# extract var at first U or V point
ncks -F -O -v $var -d ${dimUV},1 $filein ${filein}.tmpgridT
# extract var at last U or V point
ncks -F -O -v $var -d ${dimUV},$N $filein ${filein}.tmpgridT.end
# set dimUV as unlimited dimension to be able to concatenate along dimUV
ncpdq -O -a ${dimUV},${dimT} ${filein}.tmpgridT ${filein}.tmpgridT
ncpdq -O -a ${dimUV},${dimT} ${filein}.tmpgridT.end ${filein}.tmpgridT.end
ncpdq -O -a ${dimUV},${dimT} $fileout $fileout
# concatenate along dimUV the first U or V point, var at point T from indices 2 to end-1 and last U or V point
# so that we now have var on grid T (first and last T point having the value of the closest U or V point)
ncrcat -O -v $var ${filein}.tmpgridT $fileout ${filein}.tmpgridT.end $fileout
# reset time as the unlimited dimension
ncpdq -O -a ${dimT},${dimUV} $fileout $fileout

# rename dimUV to be consistent with T points
ncrename -d $dimUV,$dimRHO $fileout

# remove temporary files
rm -f ${filein}.tmpgridT ${filein}.tmpgridT.end

# remove unused dimUV dimension variable
ncks -O -x -v $dimUV $fileout $fileout

# add lon, lat and time fields
ncks -A -v $lonlattime $filein $fileout
 
