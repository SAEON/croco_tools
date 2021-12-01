#!/bin/bash


source ../../myenv_mypath.sh

WPS_DIR="${ATM}/../WPS-4.2"

#=========================================================================
#=======================  END USER CHANGES  ==============================
#=========================================================================

cp configure.wps.base ${WPS_DIR}/configure.wps

cd ${WPS_DIR}

./compile >& compile_wps.out

[[ "$2" -eq "2" ]] && { echo "Error during compilation.\n Please check ${WPS_DIR}/compile_wps.out for more informations"; exit }
[[ ! -f geogrid.exe || ! -f metgrid.exe || ! -f ungrib.exe ]] && { echo "One (or more) exe file is missing. " ; } || { echo "It is a success" ;}
