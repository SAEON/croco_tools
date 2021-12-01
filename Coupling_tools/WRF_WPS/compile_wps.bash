#!/bin/bash


source ../../myenv_mypath.sh

WPS_DIR="${ATM}/../WPS-4.2"

#=========================================================================
#=======================  END USER CHANGES  ==============================
#=========================================================================

sed -e "s|<ATM>|${ATM}|" \
    -e "s|<WRFINC>|$( nf-config --cflags )|" \
    -e "s|<WRFLIB>|$( nf-config --flibs )|" \
    -e "s|<JASPERLIB>|${JASPERLIB}|" \
    -e "s|<JASPERINC>|${JASPERINC}|" \
    -e "s|<FC>|${FC}|" -e "s|<CC>|${CC}|" \
    -e "s|<MPIF90>|${MPIF90}|" -e "s|<MPICC>|${MPICC}|" \
    ./CONFIGURE_WPS/configure.wps.${MACHINE} > ${WPS_DIR}/configure.wps

cd ${WPS_DIR}

./compile >& compile_wps.out

[[ "$2" -eq "2" ]] && { echo "Error during compilation.\n Please check ${WPS_DIR}/compile_wps.out for more informations"; exit ;}
[[ ! -f geogrid.exe || ! -f metgrid.exe || ! -f ungrib.exe ]] && { echo "One (or more) exe file is missing. " ; } || { echo "It's a success !!!" ;}
