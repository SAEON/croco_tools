#!/bin/csh
#
unalias rm
unalias mv
unalias cp
#
# Lists
#
set LIST_EXAMPLE='BASIN CANYON_A CANYON_B EQUATOR GRAV_ADJ INNERSHELF RIVER OVERFLOW SEAMOUNT SHELFRONT SOLITON UPWELLING VORTEX REGIONAL'
set LIST_KEY='AGRIF AGRIF_2WAY MPI OPENMP'
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
echo Tests serial
echo
foreach EXAMPLE ( $LIST_EXAMPLE )
  echo compile $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  jobcomp_rvtk > jobcomp_serial_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_${EXAMPLE}.exe
  echo run $EXAMPLE
  roms_${EXAMPLE}.exe > serial_${EXAMPLE}.log
  date
  grep --binary-files=text ETALON serial_${EXAMPLE}.log
  grep --binary-files=text difference: serial_${EXAMPLE}.log
end
#
# OpenMP
#
echo 
echo Tests OPEN-MP
echo
echo define OPENMP
sed 's/'undef\ OPENMP'/'define\ OPENMP'/' < cppdefs_bak1.h > cppdefs_bak2.h
/bin/mv cppdefs_bak2.h cppdefs_bak1.h
#
foreach EXAMPLE ( $LIST_EXAMPLE )
  echo compile $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  date
  jobcomp_rvtk > jobcomp_openmp_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
  /bin/mv roms roms_${EXAMPLE}.exe
  echo run $EXAMPLE
  roms_${EXAMPLE}.exe > openmp_${EXAMPLE}.log
  date
  echo test $EXAMPLE
  grep --binary-files=text ETALON openmp_${EXAMPLE}.log
  grep --binary-files=text difference: openmp_${EXAMPLE}.log
end
sed 's/'define\ OPENMP'/'undef\ OPENMP'/' < cppdefs_bak1.h > cppdefs_bak2.h
/bin/mv cppdefs_bak2.h cppdefs_bak1.h
#
# Cleaning
#
/bin/rm cppdefs_bak1.h param_bak1.h
