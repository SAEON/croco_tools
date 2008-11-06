#!/bin/csh
#
#
unalias rm
unalias mv
unalias cp
#
# Lists
#
set LIST_EXAMPLE='BASIN CANYON_A CANYON_B EQUATOR GRAV_ADJ INNERSHELF RIVER OVERFLOW SEAMOUNT SHELFRONT SOLITON UPWELLING VORTEX'
set LIST_KEY='AGRIF AGRIF_2WAY MPI OPENMP REGIONAL'
set LIST_WORDS='ETALON difference: ABNORMAL ERROR WARNING'
set SOURCE=../Roms_Agrif
#
# Get updated files
#
/bin/cp ${SOURCE}/cppdefs.h cppdefs_bak1.h
/bin/cp ${SOURCE}/param.h param_bak1.h
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
echo 
echo SERIAL TESTS
echo
foreach EXAMPLE ( $LIST_EXAMPLE )
  echo COMPILE SERIAL $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time jobcomp_rvtk > jobcomp_serial_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_${EXAMPLE}.exe
  echo RUN SERIAL $EXAMPLE
  time roms_${EXAMPLE}.exe > serial_${EXAMPLE}.log
  date
  echo TEST SERIAL $EXAMPLE
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD serial_${EXAMPLE}.log
  end
end
#
# OpenMP
#
echo 
echo define OPENMP
sed 's/'undef\ OPENMP'/'define\ OPENMP'/' < cppdefs_bak1.h > cppdefs_bak2.h
/bin/mv cppdefs_bak2.h cppdefs_bak1.h
echo
#
echo OPEN-MP 1X2 NPP=2 TESTS
echo
sed 's/'NSUB_X=2,\ NSUB_E=4,\ NPP=4'/'NSUB_X=1,\ NSUB_E=2,\ NPP=2'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
setenv OMP_NUM_THREADS 2
#
foreach EXAMPLE ( $LIST_EXAMPLE )
  echo COMPILE OPENMP 1X2 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_${EXAMPLE}.exe
  echo RUN OPENMP 1X2 $EXAMPLE
  time roms_${EXAMPLE}.exe > openmp1X2_${EXAMPLE}.log
  date
  echo TEST OPENMP 1X2  $EXAMPLE
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp1X2_${EXAMPLE}.log
  end
end
sed 's/'define\ OPENMP'/'undef\ OPENMP'/' < cppdefs_bak1.h > cppdefs_bak2.h
/bin/mv cppdefs_bak2.h cppdefs_bak1.h
#
echo
echo OPEN-MP 2X1 NPP=2 TESTS
echo
sed 's/'NSUB_X=2,\ NSUB_E=4,\ NPP=4'/'NSUB_X=2,\ NSUB_E=1,\ NPP=2'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
setenv OMP_NUM_THREADS 2
#
foreach EXAMPLE ( $LIST_EXAMPLE )
  echo COMPILE OPENMP 2X1 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_${EXAMPLE}.exe
  echo RUN OPENMP 2X1 $EXAMPLE
  time roms_${EXAMPLE}.exe > openmp2X1_${EXAMPLE}.log
  date
  echo TEST OPENMP 2X1 $EXAMPLE
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp2X1_${EXAMPLE}.log
  end
end
sed 's/'define\ OPENMP'/'undef\ OPENMP'/' < cppdefs_bak1.h > cppdefs_bak2.h
/bin/mv cppdefs_bak2.h cppdefs_bak1.h
#
echo
echo OPEN-MP 4X4 NPP=4 TESTS
echo
sed 's/'NSUB_X=2,\ NSUB_E=4,\ NPP=4'/'NSUB_X=4,\ NSUB_E=4,\ NPP=4'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
setenv OMP_NUM_THREADS 4
#
foreach EXAMPLE ( $LIST_EXAMPLE )
  echo COMPILE OPENMP 4X4 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  time jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_${EXAMPLE}.exe
  echo RUN OPENMP 4X4 $EXAMPLE
  time roms_${EXAMPLE}.exe > openmp4X4_${EXAMPLE}.log
  date
  echo TEST OPENMP 4X4 $EXAMPLE
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp4X4_${EXAMPLE}.log
  end
end
sed 's/'define\ OPENMP'/'undef\ OPENMP'/' < cppdefs_bak1.h > cppdefs_bak2.h
/bin/mv cppdefs_bak2.h cppdefs_bak1.h
#
# Cleaning
#
/bin/rm cppdefs_bak1.h param_bak1.h
