#!/bin/bash

# --------------------------------------------------
#
#  Script to prepare CFSR forcing for CROCO
#
# --------------------------------------------------
#
# Purpose: Gunzip netcdf files
#          Extract, rename and, concatenate each field in a monthly file
#          Revert latitude oerder to have it ascending from South to North
#          Rotate longitudes from 0-360 to -180-180
# 
# Inputs: Netcdf CFSR files downloaded from NCAR: e.g. 
#          pgbh06.gdas.20091016-20091020.grb2.nc.gz
#
#          Containing the following requested fields
#          - Parameter(s):
#              Temperature
#              Latent heat flux
#              Sensible heat flux
#              Specific humidity
#              Precipitation rate
#              u-component of wind
#              v-component of wind
#              Downward shortwave radiation flux
#              Upward shortwave radiation flux
#              Downward longwave radiation flux
#              Upward longwave radiation flux
#          - Vertical level(s):
#              Ground or water surface
#              Specified height above ground: 2 m
#              Specified height above ground: 10 m
#          - Product(s):
#              6-hour Forecast
#              6-hour Average (initial+0 to initial+6)
#
# Outputs: yearly folders with monthly Netcdf files per field, e.g.
#           2009 > Downward_Long-Wave_Rad_Flux_Y2009M1.nc 
#
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
# Copyright (c) 2018 G. Cambon and S. Jullien
# swen.jullien@ifremer.fr
# --------------------------------------------------


#===============================================
#          USER CHANGES
#===============================================

gunzip_flag=1
extract_flag=1
concatenate_flag=1
invertlat_flag=1
rotatelon_flag=1
adjusttime_flag=1
clean_unzip_flag=1
flag_mac=0 # 1 if you are using a mac shell (for date instructions)

date_ini="2009-01-01" # date of initialization, has to fit with tini in crocotools_param.m
yeardeb=2008
yearend=2010

dirin="$WORKDIR/DATA/CFSR_netcdf"
prefix="pgbh06.gdas"

varnameCFSR=(\
 DSWRF_L1_Avg_1 \
 DLWRF_L1_Avg_1 \
 ULWRF_L1_Avg_1 \
 USWRF_L1_Avg_1 \
 U_GRD_L103 \
 V_GRD_L103 \
 PRATE_L1_Avg_1 \
 TMP_L103 \
 SPF_H_L103 \
 TMP_L1)
varnameCROCO=(\
 Downward_Short-Wave_Rad_Flux_surface \
 Downward_Long-Wave_Rad_Flux \
 Upward_Long-Wave_Rad_Flux_surface \
 Upward_Short-Wave_Rad_Flux_surface \
 U-component_of_wind \
 V-component_of_wind \
 Precipitation_rate \
 Temperature_height_above_ground \
 Specific_humidity \
 Temperature_surface)

#===============================================
#          END USER CHANGES
#===============================================

listvarCFSR=`echo ${varnameCFSR[*]}`
listvarCROCO=`echo ${varnameCROCO[*]}`
lengthvarCFSR=${#varnameCFSR[@]}
lengthvarCROCO=${#varnameCROCO[@]}

# Check if number of input and output 
# variables are the same...
#===========================================
if [ $lengthvarCFSR -eq $lengthvarCROCO ] ; then
    echo ${lengthvarCFSR}' variables to process:'
    echo '============================'

    for  k in `seq 0 $(( ${lengthvarCFSR} - 1))` ; do
        echo $k
        vnamein=${varnameCFSR[$k]}
        vnameout=${varnameCROCO[$k]}
        echo $vnamein' ===> '$vnameout
        echo ' '
    done

else
    echo 'ERROR'
    echo 'The number of input and output variables are not the same'
    echo $lengthvarCFSR' variables for CFSR'
    echo $lengthvarCROCO' variables for CROCO'
    echo 'Check your variables in the USER CHANGES section'
    echo 'Exit'
    exit
fi

# Enter in the working directory
#===========================================
echo 'CFSR data in '$dirin
echo ' '
cd $dirin

# START loop on years...
#===========================================
for yy in `seq ${yeardeb} ${yearend}` ; do

    # Create the year dir
    #===========================================
    if [ ! -d ${dirin}/${yy} ] ; then
     echo 'Create dir. '${dirin}/${yy}
     echo ' '
     mkdir ${dirin}/${yy}
    fi

    # Gunzip NCAR CFSR files
    #===========================================
    if [ ${gunzip_flag} == 1 ] ; then
        cp *.${yy}*.nc.gz ${yy}/
        cd ${yy}/
        listf=`ls -1 ${prefix}*.${yy}*.nc.gz`
 
        for ffile in $listf ; do
            echo 'Gunzip file '$ffile
            ffile2=`echo $ffile | cut -d. -f1-4`
            gunzip ${ffile2}.nc.gz
        done

    cd ../
    fi # - gunzip_flag

    # Extract variables individually 
    # concatenate by month
    # and rename
    #===========================================
    cd ${yy}/
    listf=`ls -1 ${prefix}*.nc`

    for k in `seq 0 $(( ${lengthvarCFSR} - 1))` ; do
        var=${varnameCFSR[$k]}
        varout=${varnameCROCO[$k]}

        if [ ${extract_flag} == 1 ] ; then
            for ffile in ${listf} ; do
                echo ' '
                echo '============================'
                echo '==> ncks variable '$var
                echo ' '
                echo '... in file '$ffile

                # extract variable
                ncks -O -v ${var} ${ffile} ${var}_${ffile}

                # create time unlimited dimension
                echo '============================'
                echo '==> create unlimited time dimension '
                echo ' '
                echo '... in file '${var}_$ffile

                ## extract time dimension name
                ncdump -h ${var}_${ffile} | grep "${var}(time" > extract_time_dim_tmp
                time_dim=`cat extract_time_dim_tmp | cut -d "("  -f2 | cut -d , -f1`
                ## create unlimited time dimension
                ncecat -O ${var}_${ffile} ${var}_${ffile} 
                ncpdq -O -a $time_dim,record ${var}_${ffile} ${var}_${ffile} 
                ncwa -O -a record ${var}_${ffile} ${var}_${ffile} 
                # other way to define the dimension as unlimited dimension
                #ncks -O --mk_rec_dmn $time_dim ${var}_${ffile} ${var}_${ffile}
                ncrename -d $time_dim,time -v $time_dim,time ${var}_${ffile}
                rm -f extract_time_dim_tmp
            done # - ffile
        fi # - extract_flag

        if [ ${concatenate_flag} == 1 ] ; then
            echo '============================'
            echo '==> ncrcat variable '$var' per month'

            # START loop on months...
            #===========================================
            for month in {1..12}; do

                # concatenate...
                mm=$(printf "%02d" ${month})
                listf2=`ls -1 ${var}_*${yy}${mm}*`
                if [ ! -z ${listf2:0:1} ] ; then
                    echo 'month '$month
                    echo 'concatenate files '${listf2}
                    ncrcat -O ${listf2} -o ${var}_Y${yy}M${month}.nc
                    rm -Rf ${listf2}

                    # rename...
                    echo '============================'
                    echo '==> rename variable '$var' in '$varout
                    ncrename -O -v ${var},${varout} ${var}_Y${yy}M${month}.nc ${varout}_Y${yy}M${month}.nc
                    rm -Rf ${var}_Y${yy}M${month}.nc
               fi
            done # - month

        fi # - concatenate_flag
        
        if [ ${invertlat_flag} == 1 ] ; then
            # Invert lat...
            #===========================================
            for month in {1..12}; do
                if [ -e ${varout}_Y${yy}M${month}.nc ] ; then
                    echo '============================'
                    echo '==> invert lat for '${varout}_Y${yy}M${month}.nc
                    ncpdq -O -a -lat ${varout}_Y${yy}M${month}.nc ${varout}_Y${yy}M${month}.nc
                else 
                    echo "${varout}_Y${yy}M${month}.nc does not exist"
                fi
            done # - month
        fi # - invertlat_flag

        if [ ${rotatelon_flag} == 1 ] ; then
            # Rotate lon from 0-360 to -180-180...
            #===========================================
            for month in {1..12}; do
                if [ -e ${varout}_Y${yy}M${month}.nc ] ; then
                    echo '============================'
                    echo '==> rotate lon for '${varout}_Y${yy}M${month}.nc
                    ncks -O --msa -d lon,180.001,360. -d lon,0.,180.0 ${varout}_Y${yy}M${month}.nc ${varout}_Y${yy}M${month}.nc
                    ncap2 -O -s "where(lon > 180) lon=lon-360" ${varout}_Y${yy}M${month}.nc ${varout}_Y${yy}M${month}.nc
                    ncatted -O -a valid_range,lon,o,c,"-180.f, 180.f" ${varout}_Y${yy}M${month}.nc
                else
                    echo "${varout}_Y${yy}M${month}.nc does not exist"
                fi
            done # - month
        fi # - rotatelon_flag

        if [ ${adjusttime_flag} == 1 ] ; then
            # Set time from date_ini and in days
            #===========================================
            for month in {1..12}; do
                mm=$(printf "%02d" ${month})
                if [ -e ${varout}_Y${yy}M${month}.nc ] ; then
                    echo '============================'
                    echo '==> set time in days from initial date ' $date_ini ' in '${varout}_Y${yy}M${month}.nc
                    ncatted -O -a calendar,time,o,c,proleptic_gregorian ${varout}_Y${yy}M${month}.nc
                    ncdump -v time ${varout}_Y${yy}M${month}.nc | grep "time:units = " > extract_time_units
                    datefileorig=`cat extract_time_units | cut -d '=' -f2 | cut -c15-24`
                    if [ ${flag_mac} == 0 ] ; then
                        # For linux users
                        datefile=$(echo `date -d "$datefileorig" +%s`)
                        dateorig=$(echo `date -d "${date_ini}" +%s`)
                    else
                        # For mac users
                        datefile=$(echo `date -jf "%Y-%m-%d" "$datefileorig" +%s`)
                        dateorig=$(echo `date -jf %Y-%m-%d "${date_ini}" +%s`)
                    fi
                    dateok=$(echo `expr $datefile - $dateorig`)
                    datedays=$(( $dateok /(3600*24) ))
                    ncap2 -O -s "time=time/24 + $datedays" ${varout}_Y${yy}M${month}.nc ${varout}_Y${yy}M${month}.nc
                    ncatted -O -a units,time,o,c,"days since ${date_ini} 00:00:00" ${varout}_Y${yy}M${month}.nc
                else
                    echo "${varout}_Y${yy}M${month}.nc does not exist"
                fi
            done # - month
            rm -f extract_time_units
        fi # - adjusttime_flag

    done # - var

    if [ ${clean_unzip_flag} == 1 ] ; then
       echo ' '
       echo '==> remove the unzip files '
       echo $PWD
            listall=`ls -1 ${prefix}*.nc`
            rm -Rf ${listall}
    fi

    cd ../
done # - seq yeardeb yearend




