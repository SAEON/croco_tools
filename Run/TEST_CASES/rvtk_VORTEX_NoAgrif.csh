#!/bin/csh
#
#
unalias rm
unalias mv
unalias cp


echo "Remove *.exe* *.log* "
/bin/rm *.exe*
/bin/rm *.log*
echo "-------"

#
# Lists
#
set LIST_EXAMPLE='VORTEX'
set LIST_KEY='AGRIF AGRIF_2WAY MPI OPENMP REGIONAL'
set LIST_WORDS='ETALON difference: ABNORMAL ERROR WARNING'
set SOURCE=/home/gcambon/SVN_3/romsagrif/Roms_tools/Roms_Agrif
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
foreach KEY ( $LIST_KEY )
  echo undef $KEY
  sed 's/'define\ $KEY'/'undef\ $KEY'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
end
#
# Serial runs
#
echo "======================"
echo SERIAL TESTS
echo
/bin/cp param_bak0.h param_bak1.h
foreach EXAMPLE ( $LIST_EXAMPLE )
  echo COMPILE SERIAL $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_serial_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_serial_${EXAMPLE}.exe
  echo RUN SERIAL $EXAMPLE
  time roms_serial_${EXAMPLE}.exe > serial_${EXAMPLE}.log
  date
  echo TEST SERIAL $EXAMPLE
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD serial_${EXAMPLE}.log
  end
end
##############################################################################
# OpenMP
#############################################################################
echo 
echo define OPENMP
sed 's/'undef\ OPENMP'/'define\ OPENMP'/' < cppdefs_bak1.h > cppdefs_bak2.h
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
foreach EXAMPLE ( $LIST_EXAMPLE )
echo "--------------------------"
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE

  else 
  echo COMPILE OPENMP 1X2 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_omp1X2_${EXAMPLE}.exe
  echo RUN OPENMP 1X2 $EXAMPLE
  time roms_omp1X2_${EXAMPLE}.exe > openmp1X2_${EXAMPLE}.log
  date
  echo TEST OPENMP 1X2  $EXAMPLE
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp1X2_${EXAMPLE}.log
  end

  endif
end
#----------------------------------------------------------------------------------
echo "======================"
echo OPEN-MP 2X1 NPP=2 TESTS
echo
/bin/rm param_bak1.h
sed 's/'NSUB_X=1,\ NSUB_E=NPP'/'NSUB_X=NPP,\ NSUB_E=1'/' < param_bak0.h > param_bak1.h
sed 's/'NPP=4'/'NPP=2'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
setenv OMP_NUM_THREADS 2
#
foreach EXAMPLE ( $LIST_EXAMPLE )
echo "--------------------------"
 if ( ${EXAMPLE} == 'OVERFLOW'  || ${EXAMPLE} == 'SHELFRONT') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
 else

  echo COMPILE OPENMP 2X1 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_omp2X1_${EXAMPLE}.exe
  echo RUN OPENMP 2X1 $EXAMPLE
  time roms_omp2X1_${EXAMPLE}.exe > openmp2X1_${EXAMPLE}.log
  date
  echo TEST OPENMP 2X1 $EXAMPLE
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp2X1_${EXAMPLE}.log
  end
 endif
end
#----------------------------------------------------------------------------------
echo "======================"
echo OPEN-MP 2X2 NPP=2 TESTS
echo
/bin/rm param_bak1.h

sed 's/'NSUB_X=1,\ NSUB_E=NPP'/'NSUB_X=2,\ NSUB_E=2'/' < param_bak0.h > param_bak1.h
sed 's/'NPP=4'/'NPP=2'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
setenv OMP_NUM_THREADS 2
#
foreach EXAMPLE ( $LIST_EXAMPLE )
echo "--------------------------"
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF' || ${EXAMPLE} == 'SHELFRONT' || ${EXAMPLE} == 'OVERFLOW') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else
  
  echo COMPILE OPENMP 2X2 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_omp2X2_${EXAMPLE}.exe
  echo RUN OPENMP 2X2 $EXAMPLE
  time roms_omp2X2_${EXAMPLE}.exe > openmp2X2_${EXAMPLE}.log
  date
  echo TEST OPENMP 2X2 $EXAMPLE
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp2X2_${EXAMPLE}.log
  end

 endif
end
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
foreach EXAMPLE ( $LIST_EXAMPLE )
echo "--------------------------"
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE OPENMP 1X4 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_omp1X4_${EXAMPLE}.exe
  echo RUN OPENMP 1X4 $EXAMPLE
  time roms_omp1X4_${EXAMPLE}.exe > openmp1X4_${EXAMPLE}.log
  date
  echo TEST OPENMP 1X4 $EXAMPLE
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp1X4_${EXAMPLE}.log
  end

endif
end
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
foreach EXAMPLE ( $LIST_EXAMPLE )
echo "--------------------------"
  if ( ${EXAMPLE} == 'OVERFLOW'  || ${EXAMPLE} == 'SHELFRONT') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE OPENMP 4X1 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_omp4X1_${EXAMPLE}.exe
  echo RUN OPENMP 4X1 $EXAMPLE
  time roms_omp4X1_${EXAMPLE}.exe > openmp4X1_${EXAMPLE}.log
  date
  echo TEST OPENMP 4X1 $EXAMPLE
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp4X1_${EXAMPLE}.log
  end
endif
end
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
foreach EXAMPLE ( $LIST_EXAMPLE )
echo "--------------------------"
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF' || ${EXAMPLE} == 'OVERFLOW'  || ${EXAMPLE} == 'SHELFRONT') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE OPENMP 2X4 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_omp2X4_${EXAMPLE}.exe
  echo RUN OPENMP 2X4 $EXAMPLE
  time roms_omp2X4_${EXAMPLE}.exe > openmp2X4_${EXAMPLE}.log
  date
  echo TEST OPENMP 2X4 $EXAMPLE
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp2X4_${EXAMPLE}.log
  end
endif
end
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
foreach EXAMPLE ( $LIST_EXAMPLE )
echo "--------------------------"
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF' || ${EXAMPLE} == 'OVERFLOW'  || ${EXAMPLE} == 'SHELFRONT') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE OPENMP 4X2 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_omp4X2_${EXAMPLE}.exe
  echo RUN OPENMP 4X2 $EXAMPLE
  time roms_omp4X2_${EXAMPLE}.exe > openmp4X2_${EXAMPLE}.log
  date
  echo TEST OPENMP 4X2 $EXAMPLE
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp4X2_${EXAMPLE}.log
  end
endif
end
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
foreach EXAMPLE ( $LIST_EXAMPLE )
echo "--------------------------"
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE OPENMP 1X8 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_omp1X8_${EXAMPLE}.exe
  echo RUN OPENMP 1X8 $EXAMPLE
  time roms_omp1X8_${EXAMPLE}.exe > openmp1X8_${EXAMPLE}.log
  date
  echo TEST OPENMP 1X8 $EXAMPLE
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp1X8_${EXAMPLE}.log
  end
endif
end
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
foreach EXAMPLE ( $LIST_EXAMPLE )
echo "--------------------------"
  if ( ${EXAMPLE} == 'OVERFLOW'  || ${EXAMPLE} == 'SHELFRONT') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE OPENMP 8X1 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_omp8X1_${EXAMPLE}.exe
  echo RUN OPENMP 8X1 $EXAMPLE
  time roms_omp8X1_${EXAMPLE}.exe > openmp8X1_${EXAMPLE}.log
  date
  echo TEST OPENMP 8X1 $EXAMPLE
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp8X1_${EXAMPLE}.log
  end
endif
end
#----------------------------------------------------------------------------------


#
# Undef OPENMP
#
sed 's/'define\ OPENMP'/'undef\ OPENMP'/' < cppdefs_bak1.h > cppdefs_bak2.h
/bin/mv cppdefs_bak2.h cppdefs_bak1.h
###############################################################################
###############################################################################
# MPI
#
echo '------------------------------'
echo '---MPI TESTING MPI TESTING----'
echo '------------------------------'
echo 
echo define MPI
sed 's/'undef\ MPI'/'define\ MPI'/' < cppdefs_bak1.h > cppdefs_bak2.h
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
foreach EXAMPLE ( $LIST_EXAMPLE )
echo '----------------'
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE

  else 
  echo COMPILE MPI 1X2 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk_mpi > jobcomp_mpi_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_mpi1X2_${EXAMPLE}.exe
  echo RUN MPI 1X2 $EXAMPLE
  time mpirun -np 2 roms_mpi1X2_${EXAMPLE}.exe > mpi1X2_${EXAMPLE}.log
  date
  echo TEST MPI 1X2 $EXAMPLE
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi1X2_${EXAMPLE}.log
  end
endif
end
#----------------------------------------------------------------
echo "============="
echo MPI 2X1 TESTS
echo
/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=2,\ NP_ETA=1'/' < param_bak0.h > param_bak1.h
#
foreach EXAMPLE ( $LIST_EXAMPLE )
echo '----------------'
  if ( ${EXAMPLE} == 'OVERFLOW'  || ${EXAMPLE} == 'SHELFRONT') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else 

  echo COMPILE MPI 2X1 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk_mpi > jobcomp_mpi_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_mpi2X1_${EXAMPLE}.exe
  echo RUN MPI 2X1 $EXAMPLE
  time mpirun -np 2 roms_mpi2X1_${EXAMPLE}.exe > mpi2X1_${EXAMPLE}.log
  date
  echo TEST MPI 2X1 $EXAMPLE

  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi2X1_${EXAMPLE}.log
  end
 endif
end
#----------------------------------------------------------------
echo "============="
echo MPI 2X2 TESTS
echo
/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=2,\ NP_ETA=2'/' < param_bak0.h > param_bak1.h
#

foreach EXAMPLE ( $LIST_EXAMPLE )
echo '----------------'
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF' || ${EXAMPLE} == 'SHELFRONT'|| ${EXAMPLE} == 'OVERFLOW') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE MPI 2X2 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk_mpi > jobcomp_mpi_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_mpi2X2_${EXAMPLE}.exe
  echo RUN MPI 2X2 $EXAMPLE
  time mpirun -np 4 roms_mpi2X2_${EXAMPLE}.exe > mpi2X2_${EXAMPLE}.log
  date
  echo TEST MPI 2X2 $EXAMPLE
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi2X2_${EXAMPLE}.log
  end
 endif
end

#----------------------------------------------------------------
echo "============="
echo MPI 1X4 TESTS
echo
/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=1,\ NP_ETA=4'/' < param_bak0.h > param_bak1.h
#
foreach EXAMPLE ( $LIST_EXAMPLE )
echo '----------------'
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE MPI 1X4 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk_mpi > jobcomp_mpi_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_mpi1X4_${EXAMPLE}.exe
  echo RUN MPI 1X4 $EXAMPLE
  time mpirun -np 4 roms_mpi1X4_${EXAMPLE}.exe > mpi1X4_${EXAMPLE}.log
  date
  echo TEST MPI 1X4 $EXAMPLE
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi1X4_${EXAMPLE}.log
  end

endif
end
#----------------------------------------------------------------
echo "============="
echo MPI 4X1 TESTS
echo
/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=4,\ NP_ETA=1'/' < param_bak0.h > param_bak1.h
#
foreach EXAMPLE ( $LIST_EXAMPLE )
echo '----------------'
  if ( ${EXAMPLE} == 'OVERFLOW'  || ${EXAMPLE} == 'SHELFRONT') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE MPI 4X1 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk_mpi > jobcomp_mpi_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_mpi4X1_${EXAMPLE}.exe
  echo RUN MPI 4X1 $EXAMPLE
  time mpirun -np 4 roms_mpi4X1_${EXAMPLE}.exe > mpi4X1_${EXAMPLE}.log
  date
  echo TEST MPI 4X1 $EXAMPLE

  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi4X1_${EXAMPLE}.log
  end

endif
end
#----------------------------------------------------------------
#----------------------------------------------------------------
echo '=============='
echo MPI 2X4 TESTS
echo
/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h


sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=2,\ NP_ETA=4'/' < param_bak0.h > param_bak1.h
#

foreach EXAMPLE ( $LIST_EXAMPLE )
echo '----------------'
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF' || ${EXAMPLE} == 'OVERFLOW'  || ${EXAMPLE} == 'SHELFRONT') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE MPI 2X4 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk_mpi > jobcomp_mpi_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_mpi2X4_${EXAMPLE}.exe
  echo RUN MPI 2X4 $EXAMPLE
  time mpirun -np 8 roms_mpi2X4_${EXAMPLE}.exe > mpi2X4_${EXAMPLE}.log
  date
  echo TEST MPI 2X4 $EXAMPLE
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi2X4_${EXAMPLE}.log
  end

endif
end
#----------------------------------------------------------------
echo '=============='
echo MPI 4X2 TESTS
echo
/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=4,\ NP_ETA=2'/' < param_bak0.h > param_bak1.h
#

foreach EXAMPLE ( $LIST_EXAMPLE )
echo '----------------'
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF' || ${EXAMPLE} == 'OVERFLOW'  || ${EXAMPLE} == 'SHELFRONT') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE MPI 4X2 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk_mpi > jobcomp_mpi_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_mpi4X2_${EXAMPLE}.exe
  echo RUN MPI 4X2 $EXAMPLE
  time mpirun -np 8 roms_mpi4X2_${EXAMPLE}.exe > mpi4X2_${EXAMPLE}.log
  date
  echo TEST MPI 4X2 $EXAMPLE

  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi4X2_${EXAMPLE}.log
  end
endif
end
#----------------------------------------------------------------
echo '=============='
echo MPI 1X8 TESTS
echo
/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=1,\ NP_ETA=8'/' < param_bak0.h > param_bak1.h
#

foreach EXAMPLE ( $LIST_EXAMPLE )
echo '----------------'
  if ( ${EXAMPLE} == 'GRAV_ADJ'  || ${EXAMPLE} == 'INNERSHELF') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE MPI 1X8 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk_mpi > jobcomp_mpi_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_mpi1X8_${EXAMPLE}.exe
  echo RUN MPI 1X8 $EXAMPLE
  time mpirun -np 8 roms_mpi1X8_${EXAMPLE}.exe > mpi1X8_${EXAMPLE}.log
  date
  echo TEST MPI 1X8 $EXAMPLE

  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi1X8_${EXAMPLE}.log
  end
endif
end
#----------------------------------------------------------------
#----------------------------------------------------------------
echo '=============='
echo MPI 8X1 TESTS
echo
/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=8,\ NP_ETA=1'/' < param_bak0.h > param_bak1.h
#

foreach EXAMPLE ( $LIST_EXAMPLE )
echo '----------------'
  if ( ${EXAMPLE} == 'OVERFLOW'  || ${EXAMPLE} == 'SHELFRONT') then
   echo 'SKIP THIS TEST CASE ' $EXAMPLE
  else

  echo COMPILE MPI 8X1 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time ./jobcomp_rvtk_mpi > jobcomp_mpi_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_mpi8X1_${EXAMPLE}.exe
  echo RUN MPI 8X1 $EXAMPLE
  time mpirun -np 8 roms_mpi8X1_${EXAMPLE}.exe > mpi8X1_${EXAMPLE}.log
  date
  echo TEST MPI 8X1 $EXAMPLE

  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi8X1_${EXAMPLE}.log
  end
endif
end
#----------------------------------------------------------------
#----------------------------------------------------------------
# Cleaning
echo 'CLEANING'
/bin/rm cppdefs_bak1.h param_bak0.h param_bak1.h
