#!/bin/bash 

# --------------------------------------------------
#
#  Script to prepare WRF using the WRF Preprocessing System (WPS)
#
# --------------------------------------------------
#
# Run : ./run_wps.bash configure.namelist.wps NBPROCS
#
# Usage : 
#  - multiple nested domains (3 max currently)
#  - possible use of SST data from a different source
# 
# Dependence : 
#  - read "configure.namelist.wps" defining basic domain and 
#    run parameters
#  - Vtable : Vtable.$LBC_type
#  - Vtable : Vtable.$LSM_type    
# 
# Source grib files: eventually 
#   - run cdf2grib.sh first)
#   - run sst2grib.sh first)
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
# Copyright (c) 2007 J.Lefrevre, P. Marchesiello, S. Jullien
# swen.jullien@ifremer.fr
# adapted from F. Lemarie. LMC-IMAG, Grenoble, France
#
# Update : J. LEFEVRE, updated to use WRFV3.2 
#               Fix new SST_update control rules
#               Add Spectral nudging entries
# 
# ADD Spectral nudging (jul 2010)
#
# Modified : S. Jullien (Apr. 2016)
#            - changes in configure.namelist.wps
#            - performs wps only (not wrf) 
# --------------------------------------------------

CONFIGURE_NAMELIST=$1
NBPROCS=$2

source $CONFIGURE_NAMELIST

#=========================================================================
#=======================  USER CHANGES  ==================================
#=========================================================================

#============= Define OPTIONS for running the script =================
#
# SWITCH to define the simulation domains, and interpolate static 
# terrestrial data sets to the model grids.
# (process $WPS_ROOT/geogrid.exe) ? (1 : yes / 0 : no)
switch_geogrid=1
#
# SWITCH to plot the domains with ferret
switch_plot=0
#
# SWITCH to degribs global model data, and write the data in a simple format
# called intermediate GRIB format.
# (process $WPS_ROOT/ungrib) ? (1 : yes / 0 : no)
switch_ungrib=1
#
# SWITCH to degribs a second time from another SOURCE? (1 : yes / 0 : no)
# Example: different SST or 2nd type of surface files (e.g. CFSR)
switch_ungrib_sfc=1
#
# SWITCH to horizontally interpolates the intermediate-format meteorological 
# data that are extracted by the ungrib program onto the simulation domains 
# defined by the geogrid program. 
# (process $WPS_ROOT/metgrid) ? (1 : yes / 0 : no)
switch_metgrid=1
is_ungrib_sfc=1 # for use of a 2nd type of surface files in metgrid
#
# Flag to remove eventual old files or not
remove_flag=0
#
#============= Set Environment ======================================= 
#
source ../../myenv_mypath.sh
#
# add WPS dependencies to your library path: libpng and zlib (dynamic libraries)
##export LD_LIBRARY_PATH=$HOME/softs/libpng-1.2.59/install/lib:$LD_LIBRARY_PATH
##export LD_LIBRARY_PATH=$HOME/softs/zlib-1.2.11/install/lib:$LD_LIBRARY_PATH

# WPS paths
# WPS source dir
export WPS_EXE_DIR="${ATM}/../WPS4.2"
# Data dir
DATA_DIR=${CWORK}/DATA
#
# Geographical data for WPS
export GEOG_DATAROOT="$DATA_DIR/WPS_GEOG"
#
# Variable Table Location
#export Vtable_ROOT="${WPS_EXE_DIR}/ungrib/Variable_Tables"
export Vtable_ROOT="$PWD"
#
# Inputs directory and prefix (initial and boundary data)
export I_DATAROOT="$DATA_DIR/CFSR_grib"
export I_DATAprefix="200901*pgbh06.gdas"
# Surface Inputs directory and prefix (if different surface file)
export SFC_DATAROOT="$DATA_DIR/CFSR_grib"
export SFC_DATAprefix="200901*flxf06.gdas"
#
# Outputs data directory
export O_DATAROOT="${ATM_FILES_DIR}/WPS_DATA"
#
# Path to workdir
export MYWORKDIR="${CWORK}/outputs_wps"
if [ -e $MYWORKDIR ] ; then
 rm -f $MYWORKDIR/*
else
 mkdir -p $MYWORKDIR
fi
#
# MPI launch commands
if [ ${MACHINE} == "JEANZAY" ]; then
    export myMPI="srun -n $NBPROCS "
elif [ ${MACHINE} == "DATARMOR" ]; then
    export myMPI="$MPI_LAUNCH -np $NBPROCS "
elif [ ${MACHINE} == "IRENE" ]; then
    export myMPI="ccc_mprun -n $NBPROCS "
else
    echo "Define how to run the job in run_wps.bash"
    exit
fi
# for NEA ----------
#export myMPI="mpirun -np $NBPROCS "
# ------------------


#=========================================================================
#=======================  END USER CHANGES  ==============================
#=========================================================================

echo "  "
echo "==============================="
echo "I_DATAROOT=> "$I_DATAROOT
echo "==============================="

echo "  "
echo "==============================="
echo "O_DATAROOT=> "$O_DATAROOT
echo "==============================="

echo "  "
echo "==============================="
echo "GEOG_DATAROOT=> "$GEOG_DATAROOT
echo "==============================="

echo " "
echo "************************************************************"
echo "*                                                          *"
echo "*                This script will run the                  *"
echo "*              WRF Preprocessing System (WPS)              *"
echo "*  (P. Marchesiello, J. Lefevre, F. Lemarie, S. Jullien)   *"
echo "*                                                          *"
echo "************************************************************"

cd $MYWORKDIR

#----------------------------------------------
# Rescale parameters from configure.namelist
#----------------------------------------------
export interval_s=`expr $obc_freq_h \* 3600`
export interval_s_SFC=$interval_s
#export dx_d01=`expr $dx \* 1000`       # dx in meters
export dx_d01=`echo "$dx*1000" | bc -l`

#------------------------------------------
# Create output data directory if needed
#-------------------------------------------

  if ! [ -e $O_DATAROOT ] ; then
    mkdir -p $O_DATAROOT
  fi

#-------------------------------------------
# Remove old files
#-------------------------------------------
if [ $remove_flag -eq 1 ]; then
 if [ $switch_ungrib -eq 1 ]; then
   rm -f $O_DATAROOT/${LBC_type}:*
  if [ $switch_ungrib_sfc -eq 1 ]; then
   rm -f $O_DATAROOT/${LSM_type}:*
  fi
 fi
 if [ $switch_metgrid -eq 1 ]; then
   rm -f $O_DATAROOT/met_em*
 fi
fi

#---------------------------------------------
# Check global data files
#--------------------------------------------- 

echo ' LBC_type='$LBC_type ;  echo ' I_DATAROOT='$I_DATAROOT
list_FILES=(`ls $I_DATAROOT/*${I_DATAprefix}*`)
if [ $#{list_FILES} != "0" ]; then
  echo " BASH:  ${LBC_type} files are available"
else
  echo " BASH:  ${LBC_type} files are missing. Download them first."
  exit
fi

if [ $switch_ungrib_sfc -eq 1 ]; then
  list_SFC=(`ls $SFC_DATAROOT/*${SFC_DATAprefix}*`)
  if [ $#{list_SFC} != "0" ]; then
    echo " BASH: ${LSM_type} forcing files are available"
  else
    echo " BASH: ${LSM_type} forcing files are missing. Download them first."
    exit
  fi
fi

#--------------------------------------
# Check executables
#--------------------------------------
if [ -e $WPS_EXE_DIR/geogrid.exe -a -e $WPS_EXE_DIR/ungrib.exe -a -e $WPS_EXE_DIR/metgrid.exe ] ; then
  echo " BASH: Executables checked in $WPS_EXE_DIR"
  cp -f $WPS_EXE_DIR/geogrid.exe $MYWORKDIR/.
  cp -f $WPS_EXE_DIR/ungrib.exe $MYWORKDIR/.
  cp -f $WPS_EXE_DIR/metgrid.exe  $MYWORKDIR/.
  cp -f $WPS_EXE_DIR/link_grib.csh $MYWORKDIR/.
  echo " BASH: Executables copied to $MYWORKDIR" 
else
  echo " BASH: Executables are missing $WPS_EXE_DIR"
  exit 1
fi

# O  O  O  O  O  O  O  O  O        START geogrid      O  O  O  O  O  O  O  O  O

if [ $switch_geogrid -eq 1 ]; then
  echo " "
  echo " BASH:"
  echo " =========================================================="
  echo "                       GEOGRID                             "
  echo "                                                           "
  echo "   Localize computational domain, interpolate static data  "
  echo "   Check geogrid.log in current directory                  "
  echo " =========================================================="
  echo " "

  #------------------------------------------------
  # Create namelist.wps from configure.namelist 
  #------------------------------------------------
  if [ -e namelist.wps ] ; then
    rm -f namelist.wps
  fi

cat << End_Of_Namelist | sed -e 's/#.*//; s/  *$//' > ./namelist.wps
&share
 wrf_core = 'ARW',
 max_dom = $max_domains,
 start_date = $start_date_d01,$start_date_d02,$start_date_d03,
 end_date   = $end_date_d01,$end_date_d02,$end_date_d03,
 interval_seconds = $interval_s,
 io_form_geogrid = 2,
 opt_output_from_geogrid_path = '$O_DATAROOT',
 debug_level = 0
/

&geogrid
 parent_id         =   1,   1,  2,
 parent_grid_ratio =   1, $refine_d02, $refine_d03,
 i_parent_start    =   1, $i_str_d02, $i_str_d03,
 j_parent_start    =   1, $j_str_d02, $j_str_d03,
 e_we              =   $xdim_d01, $xdim_d02, $xdim_d03,
 e_sn              =   $ydim_d01, $ydim_d02, $ydim_d03,
 geog_data_res     =   $topo_res_d01, $topo_res_d02, $topo_res_d03,
 dx = $dx_d01,
 dy = $dx_d01,
 map_proj = 'mercator',
 ref_lat   = $central_lat
 ref_lon   = $central_lon
 truelat1  = $central_lat,
 truelat2  = 0.0,
 stand_lon = $central_lon,
 geog_data_path = '$GEOG_DATAROOT'
 opt_geogrid_tbl_path ='$WPS_EXE_DIR/geogrid'
/
End_Of_Namelist

cp namelist.wps namelist.wps.geogrid.${domain_name}

#${myMPI}geogrid.exe  >& geogrid.log
./geogrid.exe  >& geogrid.log
  echo "%  list geogrid output directory $O_DATAROOT :"
  ls $O_DATAROOT
fi

# O  O  O  O  O  O  O  O  O         END geogrid        O  O  O  O  O  O  O  O  O 
#
#
#
# O  O  O  O  O  O  O  O  O     VISUALIZE DOMAINS Embedding with Ferret    O  O  O  O  O  O  O
if [ $switch_plot -eq 1 ]; then
cd $O_DATAROOT

cat << EOF | sed -e 's/#.*//; s/  *$//' > ./Plot_Domains.jnl
! tmp.jnl  visualisation sortie de geogrid
CANCEL/ALL DATA
CANCEL/ALL VAR
CANCEL REGION

# LOOP1 BEGIN
`I=0
 while [ $I -lt $max_domains ]; do
  let I=I+1
 echo "use geo_em.d0$I.nc;"
 echo "DEFINE AXIS/Y/unit=degree_north LT$I=XLAT_M[d=$I,i=1,l=1];"
 echo "LET Xval$I = if XLONG_M[d=$I,j=1,l=1] lt 0 then XLONG_M[d=$I,j=1,l=1]+360 else XLONG_M[d=$I,j=1,l=1];"
 echo "DEFINE AXIS/X/unit=degree_east LL$I=Xval$I;"
 echo "LET H$I=RESHAPE(HGT_M[d=$I,l=1],x[gx=LL$I]+y[gy=LT$I]);"
 echo "DEFINE SYMBOL XR$I=@H$I,return=istart@;"
 echo "DEFINE SYMBOL XL$I=@H$I,return=iend@;"
 echo "DEFINE SYMBOL YU$I=@H$I,return=jstart@;"
 echo "DEFINE SYMBOL YA$I=@H$I,return=jend@;"
 echo "LET xD$I = {@x[gx=LL$I,i=(?XR$I)]@,@x[gx=LL$I,i=(?XR$I)]@,@x[gx=LL$I,i=(?XL$I)]@,@x[gx=LL$I,i=(?XL$I)]@,@x[gx=LL$I,i=(?XR$I)]@};"
 echo "LET yD$I = {@y[gy=LT$I,j=(?YU$I)]@,@y[gy=LT$I,j=(?YA$I)]@,@y[gy=LT$I,j=(?YA$I)]@,@y[gy=LT$I,j=(?YU$I)]@,@y[gy=LT$I,j=(?YU$I)]@};"
 done`
 SHADE/nolab/lev=(0,20,1),(20,100,5),(100,600,50),(600,2500,250) H1;
`I=1
 while [ $I -lt $max_domains ]; do
  let I=I+1
 echo "SHADE/nokey/nolab/over/lev=(0,20,1),(20,100,5),(100,600,50),(600,2500,250) H$I;"
 echo "PLOT/vs/over/nolab/nokey/line=1 xD$I,yD$I;"
 done`
go coastline;
frame/file=Nested_Domains.gif;

EOF

# Make some substitution in Ferret script  before execution :
 perl -pi -e "s/\@/\`/g;" Plot_Domains.jnl
 perl -pi -e "s/\?/\\$/g;" Plot_Domains.jnl
 ferret -gif -script Plot_Domains.jnl
 echo "Look my Domains : $O_DATAROOT/Nested_Domains.gif"

cd $MYWORKDIR

fi
# O  O  O  O  O  O  O  O  O   END  VISUALIZE DOMAINS     O  O  O  O  O  O  O
#
#
#
# O  O  O  O  O  O  O  O  O         START ungrib       O  O  O  O  O  O  O  O  O

if [ $switch_ungrib -eq 1 ]; then
  echo " "
  echo " BASH:"
  echo " =========================================================="
  echo "                       UNGRIB                              "
  echo "                                                           "
  echo "   read Grib files, degrib the data                        "
  echo "   and write the data in intermediate format               "
  echo " =========================================================="
  echo " "

  #-------------------------
  # Create namelist.wps
  #-------------------------
  if [ -e namelist.wps ] ; then
    rm -f namelist.wps
  fi

cat << End_Of_Namelist | sed -e 's/#.*//; s/  *$//' > ./namelist.wps
&share
 wrf_core = 'ARW',
 max_dom = $max_domains,
 start_date = $start_date_d01,$start_date_d02,$start_date_d03,
 end_date   = $end_date_d01,$end_date_d02,$end_date_d03,
 interval_seconds = $interval_s,
 io_form_geogrid = 2,
 opt_output_from_geogrid_path = '$O_DATAROOT',
 debug_level = 0
/

&ungrib
 out_format = 'WPS',
 prefix = $LBC_type,
/
End_Of_Namelist

cp namelist.wps namelist.wps.ungrib.${LBC_type}.${domain_name}

  #-----------------------------------
  # check VTABLE for LBC and make link
  #-----------------------------------
  if [ -e Vtable ] ; then
    rm -f Vtable
  fi
  echo "   Vtable :  Link to $Vtable_ROOT/Vtable.${LBC_type}"
  if [ ! -e $Vtable_ROOT/Vtable.${LBC_type} ] ; then
    echo " "
    echo "   ERROR : no Vtable.${LBC_type} found"
    echo " "
    exit 1
  fi

  ln -s $Vtable_ROOT/Vtable.${LBC_type} Vtable


  #-----------------------------------------------------
  # Create link to input grib files using link_grib.csh
  # after checking their existence
  #-----------------------------------------------------
  echo "                                                        "
  echo "   Create links to ATM Input Grib data in: $I_DATAROOT "
  echo "   Data type is ${LBC_type}                             "

  ./link_grib.csh $I_DATAROOT/*$I_DATAprefix*

  #---------------------------------------------------
  #   Execute ungrib for initial and boundary data
  #---------------------------------------------------
  echo " "
  echo "   Run ungrib for initial and boundary data"
  echo " "

 ./ungrib.exe >& ungrib.log

  mv -f ${LBC_type}:* $O_DATAROOT
  rm -f GRIBFILE.*
  echo "   ls $O_DATAROOT"
  ls $O_DATAROOT

fi

if [ $switch_ungrib_sfc -eq 1 ]; then

    #-------------------------------------------------------------
    #   Repaete procedure for secondary source of surface forcing
    #   (case switch_ungrib_sfc). first create a temporary namelist.wps
    #-------------------------------------------------------------

    if [ -e namelist.wps ] ; then
      rm -f namelist.wps
    fi

cat << End_Of_Namelist | sed -e 's/#.*//; s/  *$//' > ./namelist.wps
&share
 wrf_core = 'ARW',
 max_dom = $max_domains,
 start_date = $start_date_d01,$start_date_d02,$start_date_d03,
 end_date   = $end_date_d01,$end_date_d02,$end_date_d03,
 interval_seconds = $interval_s_SFC,
 io_form_geogrid = 2,
 opt_output_from_geogrid_path = '$O_DATAROOT',
 debug_level = 0
/
&ungrib
 out_format = 'WPS',
 prefix = $LSM_type,
/
End_Of_Namelist

cp namelist.wps namelist.wps.ungrib.${LSM_type}.${domain_name}

    #--------------------------
    # Check VTABLE for LSM 
    #---------------------------

    if [ -e Vtable ] ; then
      rm -f Vtable
    fi
    echo "Vtable :  Link to ${Vtable_ROOT}/Vtable.${LSM_type}"
    if [ ! -e ${Vtable_ROOT}/Vtable.${LSM_type} ] ; then
      echo " "
      echo "   ERROR : no Vtable.${LSM_type} found"
      echo " "
      exit 1
    fi
      ln -s ${Vtable_ROOT}/Vtable.${LSM_type} Vtable

#    #----------------------------------------------------- 
#    # Check that VTABLE for LBC has no entry for SKINTEMP
#    #-----------------------------------------------------
#    str=`cat $Vtable_ROOT/Vtable.${LBC_type} | grep -i 'SST'`
#    if [ ${#str} != "0" ];then
#      echo " "
#      echo "   ERROR : Vtable.${LBC_type} has entry SST "
#      echo "   Possible conflict with forcing specified by $LSM_type "
#      echo " "
##      exit 1
#    fi

    #-------------------------------------------------------
    # Create new link to data and run ungrib a second time
    #-------------------------------------------------------
    echo "                                                        "
    echo "   Create links to OCEAN Input Grib data in: $SFC_DATAROOT "
    echo "   Data type is ${LSM_type}                             "
    echo "   ./link_grib.csh $SFC_DATAROOT/*${SFC_DATAprefix}* "
    ./link_grib.csh $SFC_DATAROOT/*${SFC_DATAprefix}*

    echo " "
    echo " Run ungrib for surface forcing data " 
    ./ungrib.exe >& ungrib_${LSM_type}.log

    mv -f ${LSM_type}:* $O_DATAROOT
    rm -f GRIBFILE.*
    echo "   ls $O_DATAROOT"
    ls $O_DATAROOT

fi # End switch_ungrib_SFC


# O  O  O  O  O  O  O  O  O         END ungrib          O  O  O  O  O  O  O  O  O 
#
#
#
# O  O  O  O  O  O  O  O  O        START metgrid        O  O  O  O  O  O  O  O  O
 
if [ $switch_metgrid -eq 1 ]; then
  echo " "
  echo " BASH:"
  echo " =========================================================="
  echo "                        METGRID                            "
  echo "                                                           "
  echo "  horizontally interpolate external data on WRF            "
  echo "  computational grid. Check metgrid.log in current         "
  echo "  directory for diagnostics                                "
  echo " =========================================================="
  echo " "

  if [ -e namelist.wps ] ; then
    rm -f namelist.wps
  fi

 # Add path to LSM in "fg_name" if switch_ungrib_sfc is up
 if [ $is_ungrib_sfc -eq 1 ]; then
   fg_name="fg_name                      = '$O_DATAROOT/${LBC_type}','$O_DATAROOT/${LSM_type}'"
 else
   fg_name="fg_name                      = '$O_DATAROOT/${LBC_type}'"
 fi

 cat << End_Of_Namelist | sed -e 's/#.*//; s/  *$//' > ./namelist.wps
&share
 wrf_core = 'ARW',
 max_dom = $max_domains,
 start_date = $start_date_d01,$start_date_d02,$start_date_d03,
 end_date   = $end_date_d01,$end_date_d02,$end_date_d03,
 interval_seconds = $interval_s,
 io_form_geogrid = 2,
 opt_output_from_geogrid_path = '$O_DATAROOT',
 debug_level =0
/
&metgrid
 $fg_name
 io_form_metgrid              = 2,
 opt_output_from_metgrid_path = '$O_DATAROOT',
 opt_metgrid_tbl_path         = '$WPS_EXE_DIR/metgrid',
/
End_Of_Namelist

cp namelist.wps namelist.wps.metgrid.${domain_name}

#${myMPI}metgrid.exe >& metgrid.log
./metgrid.exe >& metgrid.log

  echo "   ls -rtl $O_DATAROOT"
  ls -rtl $O_DATAROOT*

fi

# O  O  O  O  O  O  O  O  O         END metgrid          O  O  O  O  O  O  O  O  O 

