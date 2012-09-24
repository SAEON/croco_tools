#!/bin/csh
#
#======================================================================
# ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
# The two other branches from UCLA (Shchepetkin et al) 
# and Rutgers University (Arango et al) are under MIT/X style license.
# ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
# 
# ROMS_AGRIF website : http://roms.mpl.ird.fr
#======================================================================
#
#---------------------------------------------------------------------
# Script to Run RVTK DEBUG procedure managing parallelization type 
# AND AGRIF nesting type (No nesting, Nesting 1-way, Nesting 2-ways) : 
# VORTEX and REGIONAL
#--------------------------------------------------------------------
unalias rm
unalias mv
unalias cp

set MPIRUN=/usr/local/openmpi-1.4.3-gfortran/bin/mpirun
#set MPIRUN=/usr/local/openmpi-1.4.3-intel11/bin/mpirun
echo "==========================="
echo "MPIRUN COMMAND: "$MPIRUN
echo "==========================="
echo "Remove *.exe* *.log* "
/bin/rm *.exe*
/bin/rm *.log*
echo "Remove the CHECKFILE"
/bin/rm check_file
echo " "
#
# Lists
#
#set LIST_EXAMPLE='BASIN CANYON_A CANYON_B EQUATOR GRAV_ADJ INNERSHELF OVERFLOW SEAMOUNT SHELFRONT SOLITON UPWELLING'

set LIST_EXAMPLE='REGIONAL'
set LIST_KEY='AGRIF AGRIF_2WAY MPI OPENMP REGIONAL BENGUELA_LR ETALON_CHECK'
set LIST_WORDS='ETALON difference: ABNORMAL ERROR BUGBIN LEVELBUG'
#
# Type of parallelization
#
set COMPOMP='ON'
set COMPMPI='ON'
echo ' '
#
echo 'OpenMP testing: '$COMPOMP
echo 'MPI    testing: '$COMPMPI
echo ' '
#
# Type of Nesting (in case of VORTEX OR BENGUELA_LR/VHR)
#
set LIST_AGRIFFLAG='OFF 1W 2W'
#set LIST_AGRIFFLAG='1W'
echo 'Agrif type testing: '$LIST_AGRIFFLAG
echo ' '
#
# Name of the regional configuration
#
set CONFIG_NAME='BENGUELA_VHR'
echo 'CONFIG NAME: '$CONFIG_NAME
echo ' '
#
# Sources
#
#set SOURCE=/home/gcambon/SVN_3/romsagrif/Roms_tools/Roms_Agrif/
set SOURCE=$SOURCE_ROMS
echo 'Sources code: '$SOURCE
echo ' '
#
# Get updated files
#
/bin/cp ${SOURCE}/cppdefs.h cppdefs_bak1.h
/bin/cp ${SOURCE}/param.h param_bak0.h
#
# Title
#
echo TESTS OF $LIST_EXAMPLE
#
# Undef all examples
#
foreach EXAMPLE ( $LIST_EXAMPLE )
  echo undef $EXAMPLE
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
end
#
# Undef AGRIF, MPI, OPENMP etc..
#
echo " "
echo "UNDEF THE KEYS IN LIST_KEYS"
foreach KEY ( $LIST_KEY )
  echo undef $KEY
  sed 's/'define\ $KEY'/'undef\ $KEY'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
end
#
# Define DEBUG keys
#
echo " "
echo "==========================="
echo "TEST RVTK_DEBUG"
echo "==========================="
sed 's/'undef\ \ \*RVTK_DEBUG'/'define\ RVTK_DEBUG'/' < cppdefs_bak1.h > cppdefs_bak2.h
/bin/mv cppdefs_bak2.h cppdefs_bak1.h

#
# Define BENGUELA_VHR keys
#
echo "==========================="
echo "BENGUELA_VHR"
echo "==========================="
sed 's/'undef\ \ \*BENGUELA_LR'/'define\ $CONFIG_NAME'/' < cppdefs_bak1.h > cppdefs_bak2.h
/bin/mv cppdefs_bak2.h cppdefs_bak1.h


echo " "
echo "Debut de la boucle sur les cas tests"
 foreach EXAMPLE ( $LIST_EXAMPLE )
#Define the right AGRIF_FixedGrids.in
    if  (${EXAMPLE} == 'VORTEX') then      
      /bin/cp -Rf AGRIF_FixedGrids.in.VORTEX AGRIF_FixedGrids.in 
    else 
      /bin/cp -Rf AGRIF_FixedGrids.in.REGIONAL AGRIF_FixedGrids.in
    endif

  foreach AGRIFFLAG ( $LIST_AGRIFFLAG )
echo "CAS TEST # "$EXAMPLE
#
# Serial runs
#
echo "======================"
echo SERIAL TESTS
echo "Remove the CHECKFILE"
/bin/rm check_file
echo " "

echo "Undef NPP > 1"
sed 's/'NSUB_X=1,\ NSUB_E=NPP'/'NSUB_X=1,\ NSUB_E=1'/' < param_bak0.h > param_bak1.h
sed 's/'NPP=1'/'NPP=1'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
echo " "

echo "undef MPI"
sed 's/'define\ MPI'/'undef\ MPI'/' < cppdefs_bak1.h > cppdefs_bak2.h
/bin/mv cppdefs_bak2.h cppdefs_bak1.h
echo " "

echo "undef OPENMP"
sed 's/'define\ OPENMP'/'undef\ OPENMP'/' < cppdefs_bak1.h > cppdefs_bak2.h
/bin/mv cppdefs_bak2.h cppdefs_bak1.h
echo " "

#
# AGRIF 1W AGRIF 2W
# 

           if (${AGRIFFLAG} == 'OFF') then      
             echo "AGRIF OFF"
             sed 's/'define\ AGRIF'/'undef\ AGRIF'/' < cppdefs_bak1.h > cppdefs_bak2.h
             /bin/mv cppdefs_bak2.h cppdefs_bak1.h
           endif

           if (${AGRIFFLAG} == '1W') then      
             echo "AGRIF 1W"
             sed 's/'undef\ \ \*AGRIF'/'define\ AGRIF'/' < cppdefs_bak1.h > cppdefs_bak2.h
             sed 's/'define\ AGRIF_2WAY'/'undef\ AGRIF_2WAY'/' < cppdefs_bak2.h > cppdefs_bak3.h
             /bin/mv cppdefs_bak3.h cppdefs_bak1.h
           endif

             if (${AGRIFFLAG} == '2W') then
             echo "AGRIF 2W"
             sed 's/'undef\ \ \*AGRIF'/'define\ AGRIF'/' < cppdefs_bak1.h > cppdefs_bak2.h
             /bin/mv cppdefs_bak2.h cppdefs_bak1.h
           endif
 
/bin/cp param_bak0.h param_bak1.h
  echo COMPILE SERIAL $EXAMPLE
  sed 's/'undef\ \ \*$EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_serial_${EXAMPLE}_${AGRIFFLAG}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_serial_${EXAMPLE}_${AGRIFFLAG}.exe
  echo RUN SERIAL $EXAMPLE
  time ./roms_serial_${EXAMPLE}_${AGRIFFLAG}.exe > serial_${EXAMPLE}_${AGRIFFLAG}.log
  date
  echo TEST SERIAL $EXAMPLE AGRIF: $AGRIFFLAG
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD serial_${EXAMPLE}_${AGRIFFLAG}.log
  end
/bin/rm *.nc



if  ( ${COMPOMP} == 'ON' ) then
##############################################################################
# OpenMP
#############################################################################
echo 
echo define OPENMP
sed 's/'undef\ \ \*OPENMP'/'define\ OPENMP'/' < cppdefs_bak1.h > cppdefs_bak2.h
/bin/mv cppdefs_bak2.h cppdefs_bak1.h

#---------------------------------------------------------------------------------
echo "======================"
echo OPEN-MP 1X2 NPP=2 TESTS
echo
/bin/rm param_bak1.h
sed 's/'NSUB_X=1,\ NSUB_E=NPP'/'NSUB_X=1,\ NSUB_E=NPP'/' < param_bak0.h > param_bak1.h
sed 's/'NPP=4'/'NPP=2'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
setenv OMP_NUM_THREADS 2
#

echo "--------------------------"
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE

  else 
  echo COMPILE OPENMP 1X2 $EXAMPLE 
  sed 's/'undef\ \ \*$EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}_${AGRIFFLAG}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_omp1X2_${EXAMPLE}_${AGRIFFLAG}.exe
  echo RUN OPENMP 1X2 $EXAMPLE
  time ./roms_omp1X2_${EXAMPLE}_${AGRIFFLAG}.exe > openmp1X2_${EXAMPLE}_${AGRIFFLAG}.log
  date
  echo TEST OPENMP 1X2  $EXAMPLE AGRIF: $AGRIFFLAG
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp1X2_${EXAMPLE}_${AGRIFFLAG}.log
  end

  endif
/bin/rm *.nc


#------------------------------------------------------------------------------
echo "======================"
echo OPEN-MP 2X1 NPP=2 TESTS
echo
/bin/rm param_bak1.h
sed 's/'NSUB_X=1,\ NSUB_E=NPP'/'NSUB_X=NPP,\ NSUB_E=1'/' < param_bak0.h > param_bak1.h
sed 's/'NPP=2'/'NPP=2'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
setenv OMP_NUM_THREADS 2
#
echo "--------------------------"
 if ( ${EXAMPLE} == 'OVERFLOW'  || ${EXAMPLE} == 'SHELFRONT') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
 else

  echo COMPILE OPENMP 2X1 $EXAMPLE
  sed 's/'undef\ \ \*$EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}_${AGRIFFLAG}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_omp2X1_${EXAMPLE}_${AGRIFFLAG}.exe
  echo RUN OPENMP 2X1 $EXAMPLE
  time ./roms_omp2X1_${EXAMPLE}_${AGRIFFLAG}.exe > openmp2X1_${EXAMPLE}_${AGRIFFLAG}.log
  date
  echo TEST OPENMP 2X1 $EXAMPLE AGRIF: $AGRIFFLAG
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp2X1_${EXAMPLE}_${AGRIFFLAG}.log
  end
 endif
/bin/rm *.nc

#------------------------------------------------------------------------------
echo "======================"
echo OPEN-MP 2X2 NPP=2 TESTS
echo
/bin/rm param_bak1.h

sed 's/'NSUB_X=1,\ NSUB_E=NPP'/'NSUB_X=2,\ NSUB_E=2'/' < param_bak0.h > param_bak1.h
sed 's/'NPP=4'/'NPP=2'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
setenv OMP_NUM_THREADS 2
#
echo "--------------------------"
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF' || ${EXAMPLE} == 'SHELFRONT' || ${EXAMPLE} == 'OVERFLOW') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else
  
  echo COMPILE OPENMP 2X2 $EXAMPLE
  sed 's/'undef\ \ \*$EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}_${AGRIFFLAG}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_omp2X2_${EXAMPLE}_${AGRIFFLAG}.exe
  echo RUN OPENMP 2X2 $EXAMPLE
  time ./roms_omp2X2_${EXAMPLE}_${AGRIFFLAG}.exe > openmp2X2_${EXAMPLE}_${AGRIFFLAG}.log
  date
  echo TEST OPENMP 2X2 $EXAMPLE AGRIF: $AGRIFFLAG
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp2X2_${EXAMPLE}_${AGRIFFLAG}.log
  end

 endif
/bin/rm *.nc
#----------------------------------------------------------------------------------
echo "======================"
echo OPEN-MP 1X4 NPP=4 TESTS
echo
/bin/rm param_bak1.h

sed 's/'NSUB_X=1,\ NSUB_E=NPP'/'NSUB_X=1,\ NSUB_E=4'/' < param_bak0.h > param_bak1.h
sed 's/'NPP=4'/'NPP=4'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
setenv OMP_NUM_THREADS 4
#
echo "--------------------------"
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE OPENMP 1X4 $EXAMPLE
  sed 's/'undef\ \ \*$EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}_${AGRIFFLAG}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_omp1X4_${EXAMPLE}_${AGRIFFLAG}.exe
  echo RUN OPENMP 1X4 $EXAMPLE
  time ./roms_omp1X4_${EXAMPLE}_${AGRIFFLAG}.exe > openmp1X4_${EXAMPLE}_${AGRIFFLAG}.log
  date
  echo TEST OPENMP 1X4 $EXAMPLE AGRIF: $AGRIFFLAG
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp1X4_${EXAMPLE}_${AGRIFFLAG}.log
  end

  endif
/bin/rm *.nc
#----------------------------------------------------------------------------------
echo "======================"
echo OPEN-MP 4X1 NPP=4 TESTS
echo
/bin/rm param_bak1.h

sed 's/'NSUB_X=1,\ NSUB_E=NPP'/'NSUB_X=4,\ NSUB_E=1'/' < param_bak0.h > param_bak1.h
sed 's/'NPP=4'/'NPP=4'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
setenv OMP_NUM_THREADS 4
#
echo "--------------------------"
  if ( ${EXAMPLE} == 'OVERFLOW'  || ${EXAMPLE} == 'SHELFRONT') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE OPENMP 4X1 $EXAMPLE
  sed 's/'undef\ \ \*$EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}_${AGRIFFLAG}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_omp4X1_${EXAMPLE}_${AGRIFFLAG}.exe
  echo RUN OPENMP 4X1 $EXAMPLE
  time ./roms_omp4X1_${EXAMPLE}_${AGRIFFLAG}.exe > openmp4X1_${EXAMPLE}_${AGRIFFLAG}.log
  date
  echo TEST OPENMP 4X1 $EXAMPLE AGRIF: $AGRIFFLAG
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp4X1_${EXAMPLE}_${AGRIFFLAG}.log
  end
endif
/bin/rm *.nc
#----------------------------------------------------------------------------------
echo "======================"
echo OPEN-MP 2X4 NPP=8 TESTS
echo
/bin/rm param_bak1.h

sed 's/'NSUB_X=1,\ NSUB_E=NPP'/'NSUB_X=2,\ NSUB_E=4'/' < param_bak0.h > param_bak1.h
sed 's/'NPP=4'/'NPP=8'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
setenv OMP_NUM_THREADS 8
#
echo "--------------------------"
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF' || ${EXAMPLE} == 'OVERFLOW'  || ${EXAMPLE} == 'SHELFRONT') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE OPENMP 2X4 $EXAMPLE 
  sed 's/'undef\ \ \*$EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}_${AGRIFFLAG}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_omp2X4_${EXAMPLE}_${AGRIFFLAG}.exe
  echo RUN OPENMP 2X4 $EXAMPLE
  time ./roms_omp2X4_${EXAMPLE}_${AGRIFFLAG}.exe > openmp2X4_${EXAMPLE}_${AGRIFFLAG}.log
  date
  echo TEST OPENMP 2X4 $EXAMPLE AGRIF: $AGRIFFLAG
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp2X4_${EXAMPLE}_${AGRIFFLAG}.log
  end
endif
/bin/rm *.nc
#----------------------------------------------------------------------------------
echo "======================"
echo OPEN-MP 4X2 NPP=2 TESTS
echo
/bin/rm param_bak1.h

sed 's/'NSUB_X=1,\ NSUB_E=NPP'/'NSUB_X=4,\ NSUB_E=2'/' < param_bak0.h > param_bak1.h
sed 's/'NPP=4'/'NPP=8'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
setenv OMP_NUM_THREADS 8
#
echo "--------------------------"
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF' || ${EXAMPLE} == 'OVERFLOW'  || ${EXAMPLE} == 'SHELFRONT') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE OPENMP 4X2 $EXAMPLE
  sed 's/'undef\ \ \*$EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}_${AGRIFFLAG}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_omp4X2_${EXAMPLE}_${AGRIFFLAG}.exe
  echo RUN OPENMP 4X2 $EXAMPLE
  time ./roms_omp4X2_${EXAMPLE}_${AGRIFFLAG}.exe > openmp4X2_${EXAMPLE}_${AGRIFFLAG}.log
  date
  echo TEST OPENMP 4X2 $EXAMPLE AGRIF: $AGRIFFLAG
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp4X2_${EXAMPLE}_${AGRIFFLAG}.log
  end
endif
/bin/rm *.nc

#----------------------------------------------------------------------------------
echo "======================"
echo OPEN-MP 1X8 NPP=8 TESTS
echo
/bin/rm param_bak1.h

sed 's/'NSUB_X=1,\ NSUB_E=NPP'/'NSUB_X=1,\ NSUB_E=8'/' < param_bak0.h > param_bak1.h
sed 's/'NPP=4'/'NPP=8'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
setenv OMP_NUM_THREADS 8
#
echo "--------------------------"
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE OPENMP 1X8 $EXAMPLE
  sed 's/'undef\ \ \*$EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}_${AGRIFFLAG}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_omp1X8_${EXAMPLE}_${AGRIFFLAG}.exe
  echo RUN OPENMP 1X8 $EXAMPLE
  time ./roms_omp1X8_${EXAMPLE}_${AGRIFFLAG}.exe > openmp1X8_${EXAMPLE}_${AGRIFFLAG}.log
  date
  echo TEST OPENMP 1X8 $EXAMPLE AGRIF: $AGRIFFLAG
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp1X8_${EXAMPLE}_${AGRIFFLAG}.log
  end
endif
/bin/rm *.nc
#----------------------------------------------------------------------------------
echo "======================"
echo OPEN-MP 8X1 NPP=8 TESTS
echo
/bin/rm param_bak1.h

sed 's/'NSUB_X=1,\ NSUB_E=NPP'/'NSUB_X=8,\ NSUB_E=1'/' < param_bak0.h > param_bak1.h
sed 's/'NPP=4'/'NPP=8'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
setenv OMP_NUM_THREADS 8
#

echo "--------------------------"
  if ( ${EXAMPLE} == 'OVERFLOW'  || ${EXAMPLE} == 'SHELFRONT') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE OPENMP 8X1 $EXAMPLE
  sed 's/'undef\ \ \*$EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}_${AGRIFFLAG}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_omp8X1_${EXAMPLE}_${AGRIFFLAG}.exe
  echo RUN OPENMP 8X1 $EXAMPLE
  time ./roms_omp8X1_${EXAMPLE}_${AGRIFFLAG}.exe > openmp8X1_${EXAMPLE}_${AGRIFFLAG}.log
  date
  echo TEST OPENMP 8X1 $EXAMPLE AGRIF: $AGRIFFLAG
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp8X1_${EXAMPLE}_${AGRIFFLAG}.log
  end
endif
/bin/rm *.nc
#----------------------------------------------------------------------------------
endif
#COMPOMP

#
# Undef OPENMP
#
sed 's/'define\ OPENMP'/'undef\ OPENMP'/' < cppdefs_bak1.h > cppdefs_bak2.h
/bin/mv cppdefs_bak2.h cppdefs_bak1.h
###############################################################################


if ( ${COMPMPI} == 'ON' ) then
###############################################################################
# MPI
#
echo '------------------------------'
echo '---MPI TESTING MPI TESTING----'
echo '------------------------------'
echo 
echo define MPI
sed 's/'undef\ \ \*MPI'/'define\ MPI'/' < cppdefs_bak1.h > cppdefs_bak2.h
/bin/mv cppdefs_bak2.h cppdefs_bak1.h
echo
#
echo "============="
echo MPI 1X2 TESTS
echo
/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=1,\ NP_ETA=2'/' < param_bak0.h > param_bak1.h
#

echo '----------------'
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE

  else 
  echo COMPILE MPI 1X2 $EXAMPLE
  sed 's/'undef\ \ \*$EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_mpi_${EXAMPLE}_${AGRIFFLAG}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_mpi1X2_${EXAMPLE}_${AGRIFFLAG}.exe
  echo RUN MPI 1X2 $EXAMPLE
  time $MPIRUN -np 2 roms_mpi1X2_${EXAMPLE}_${AGRIFFLAG}.exe > mpi1X2_${EXAMPLE}_${AGRIFFLAG}.log
  date
  echo TEST MPI 1X2 $EXAMPLE AGRIF: $AGRIFFLAG
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi1X2_${EXAMPLE}_${AGRIFFLAG}.log
  end
endif

/bin/rm *.nc
#----------------------------------------------------------------
echo "============="
echo MPI 2X1 TESTS
echo
/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=2,\ NP_ETA=1'/' < param_bak0.h > param_bak1.h
#

echo '----------------'
  if ( ${EXAMPLE} == 'OVERFLOW'  || ${EXAMPLE} == 'SHELFRONT') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else 

  echo COMPILE MPI 2X1 $EXAMPLE
  sed 's/'undef\ \ \*$EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_mpi_${EXAMPLE}_${AGRIFFLAG}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_mpi2X1_${EXAMPLE}_${AGRIFFLAG}.exe
  echo RUN MPI 2X1 $EXAMPLE
  time $MPIRUN -np 2 roms_mpi2X1_${EXAMPLE}_${AGRIFFLAG}.exe > mpi2X1_${EXAMPLE}_${AGRIFFLAG}.log
  date
  echo TEST MPI 2X1 $EXAMPLE AGRIF: $AGRIFFLAG
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi2X1_${EXAMPLE}_${AGRIFFLAG}.log
  end

 endif

/bin/rm *.nc
#----------------------------------------------------------------
echo "============="
echo MPI 2X2 TESTS
echo
/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=2,\ NP_ETA=2'/' < param_bak0.h > param_bak1.h
#


echo '----------------'
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF' || ${EXAMPLE} == 'SHELFRONT'|| ${EXAMPLE} == 'OVERFLOW') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE MPI 2X2 $EXAMPLE
  sed 's/'undef\ \ \*$EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_mpi_${EXAMPLE}_${AGRIFFLAG}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_mpi2X2_${EXAMPLE}_${AGRIFFLAG}.exe
  echo RUN MPI 2X2 $EXAMPLE
  time $MPIRUN -np 4 roms_mpi2X2_${EXAMPLE}_${AGRIFFLAG}.exe > mpi2X2_${EXAMPLE}_${AGRIFFLAG}.log
  date
  echo TEST MPI 2X2 $EXAMPLE AGRIF: $AGRIFFLAG
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi2X2_${EXAMPLE}_${AGRIFFLAG}.log
  end
 endif

/bin/rm *.nc
#----------------------------------------------------------------
echo "============="
echo MPI 1X4 TESTS
echo
/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=1,\ NP_ETA=4'/' < param_bak0.h > param_bak1.h
#

echo '----------------'
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE MPI 1X4 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_mpi_${EXAMPLE}_${AGRIFFLAG}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_mpi1X4_${EXAMPLE}_${AGRIFFLAG}.exe
  echo RUN MPI 1X4 $EXAMPLE
  time $MPIRUN -np 4 roms_mpi1X4_${EXAMPLE}_${AGRIFFLAG}.exe > mpi1X4_${EXAMPLE}_${AGRIFFLAG}.log
  date
  echo TEST MPI 1X4 $EXAMPLE AGRIF: $AGRIFFLAG
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi1X4_${EXAMPLE}_${AGRIFFLAG}.log
  end

endif

/bin/rm *.nc
#----------------------------------------------------------------
echo "============="
echo MPI 4X1 TESTS
echo
/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=4,\ NP_ETA=1'/' < param_bak0.h > param_bak1.h
#

echo '----------------'
  if ( ${EXAMPLE} == 'OVERFLOW'  || ${EXAMPLE} == 'SHELFRONT') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE MPI 4X1 $EXAMPLE
  sed 's/'undef\ \ \*$EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_mpi_${EXAMPLE}_${AGRIFFLAG}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_mpi4X1_${EXAMPLE}_${AGRIFFLAG}.exe
  echo RUN MPI 4X1 $EXAMPLE
  time $MPIRUN -np 4 roms_mpi4X1_${EXAMPLE}_${AGRIFFLAG}.exe > mpi4X1_${EXAMPLE}_${AGRIFFLAG}.log
  date
  echo TEST MPI 4X1 $EXAMPLE AGRIF: $AGRIFFLAG
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi4X1_${EXAMPLE}_${AGRIFFLAG}.log
  end

endif

/bin/rm *.nc
#----------------------------------------------------------------
#----------------------------------------------------------------
echo '=============='
echo MPI 2X4 TESTS
echo
/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h


sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=2,\ NP_ETA=4'/' < param_bak0.h > param_bak1.h
#


echo '----------------'
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF' || ${EXAMPLE} == 'OVERFLOW'  || ${EXAMPLE} == 'SHELFRONT') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE MPI 2X4 $EXAMPLE
  sed 's/'undef\ \ \*$EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_mpi_${EXAMPLE}_${AGRIFFLAG}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_mpi2X4_${EXAMPLE}_${AGRIFFLAG}.exe
  echo RUN MPI 2X4 $EXAMPLE
  time $MPIRUN -np 8 roms_mpi2X4_${EXAMPLE}_${AGRIFFLAG}.exe > mpi2X4_${EXAMPLE}_${AGRIFFLAG}.log
  date
  echo TEST MPI 2X4 $EXAMPLE AGRIF: $AGRIFFLAG
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi2X4_${EXAMPLE}_${AGRIFFLAG}.log
  end

endif

/bin/rm *.nc
#----------------------------------------------------------------
echo '=============='
echo MPI 4X2 TESTS
echo
/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=4,\ NP_ETA=2'/' < param_bak0.h > param_bak1.h
#


echo '----------------'
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF' || ${EXAMPLE} == 'OVERFLOW'  || ${EXAMPLE} == 'SHELFRONT') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE MPI 4X2 $EXAMPLE
  sed 's/'undef\ \ \*$EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_mpi_${EXAMPLE}_${AGRIFFLAG}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_mpi4X2_${EXAMPLE}_${AGRIFFLAG}.exe
  echo RUN MPI 4X2 $EXAMPLE
  time $MPIRUN -np 8 roms_mpi4X2_${EXAMPLE}_${AGRIFFLAG}.exe > mpi4X2_${EXAMPLE}_${AGRIFFLAG}.log
  date
  echo TEST MPI 4X2 $EXAMPLE AGRIF: $AGRIFFLAG
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi4X2_${EXAMPLE}_${AGRIFFLAG}.log
  end
endif

/bin/rm *.nc
#----------------------------------------------------------------
echo '=============='
echo MPI 1X8 TESTS
echo
/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=1,\ NP_ETA=8'/' < param_bak0.h > param_bak1.h
#


echo '----------------'
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE MPI 1X8 $EXAMPLE
  sed 's/'undef\ \ \*$EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_mpi_${EXAMPLE}_${AGRIFFLAG}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_mpi1X8_${EXAMPLE}_${AGRIFFLAG}.exe
  echo RUN MPI 1X8 $EXAMPLE
  time $MPIRUN -np 8 roms_mpi1X8_${EXAMPLE}_${AGRIFFLAG}.exe > mpi1X8_${EXAMPLE}_${AGRIFFLAG}.log
  date
  echo TEST MPI 1X8 $EXAMPLE AGRIF: $AGRIFFLAG
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi1X8_${EXAMPLE}_${AGRIFFLAG}.log
  end
endif

/bin/rm *.nc
#----------------------------------------------------------------
#----------------------------------------------------------------
echo '=============='
echo MPI 8X1 TESTS
echo
/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=8,\ NP_ETA=1'/' < param_bak0.h > param_bak1.h
#


echo '----------------'
  if ( ${EXAMPLE} == 'OVERFLOW'  || ${EXAMPLE} == 'SHELFRONT') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE MPI 8X1 $EXAMPLE
  sed 's/'undef\ \ \*$EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_mpi_${EXAMPLE}_${AGRIFFLAG}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_mpi8X1_${EXAMPLE}_${AGRIFFLAG}.exe
  echo RUN MPI 8X1 $EXAMPLE
  time $MPIRUN -np 8 roms_mpi8X1_${EXAMPLE}_${AGRIFFLAG}.exe > mpi8X1_${EXAMPLE}_${AGRIFFLAG}.log
  date
  echo TEST MPI 8X1 $EXAMPLE AGRIF: $AGRIFFLAG
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi8X1_${EXAMPLE}_${AGRIFFLAG}.log
  end
endif

/bin/rm *.nc
echo "Fin Cas Test "$EXAMPLE
#----------------------------------------------------------------
endif
# COMPMPI

 end
#boucle sur type d'agrif
end   
#boucle sur les cas tests VORTEX ou REGIONAL

#----------------------------------------------------------------
#----------------------------------------------------------------
# Cleaning
echo " "
echo "End of the RVTK PROCEDURE"
echo "CLEANING CHECk FILE"
#/bin/rm cppdefs_bak1.h param_bak0.h param_bak1.h
/bin/rm check_file
