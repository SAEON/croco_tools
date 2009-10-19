#!/bin/csh
#
#
unalias rm
unalias mv
unalias cp

echo "Remove *.exe* *.log* "
/bin/rm *.exe* *.log*
echo "--------------------"


#
# Lists
#
set LIST_EXAMPLE='VORTEX'
set LIST_KEY='AGRIF AGRIF_2WAY MPI OPENMP REGIONAL'
set LIST_KEY_AGRIF='AGRIF'
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

#########################################################################################
# AGRIF 1W et AGRIF 2W
set   Nesting2W=1
echo '2WAYS NESTING FLAG = ' $Nesting2W
if ( $Nesting2W == 0 ) then
   echo 'AGRIF 1 Ways'
else
   echo 'AGRIF 2 Ways'
endif
#########################################################################################
#
#
#


#########################################################################################
########################################################################################
#
# Serial runs
#
echo "======================"
echo SERIAL TESTS
echo
/bin/cp param_bak0.h param_bak1.h
foreach EXAMPLE ( $LIST_EXAMPLE )
 foreach KEY_AGRIF ( $LIST_KEY_AGRIF )
  echo COMPILE SERIAL $EXAMPLE - $KEY_AGRIF
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'undef\ $KEY_AGRIF'/'define\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

if ( $Nesting2W == 0 ) then
  sed s/'define AGRIF_2WAY'/'undef AGRIF_2WAY'/ < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
endif

  date
  time ./jobcomp_rvtk > jobcomp_serial_${EXAMPLE}_${KEY_AGRIF}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'define\ $KEY_AGRIF'/'undef\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h
  /bin/mv roms roms_serial_${EXAMPLE}_${KEY_AGRIF}.exe
  echo RUN SERIAL $EXAMPLE - ${KEY_AGRIF}
  time roms_serial_${EXAMPLE}_${KEY_AGRIF}.exe > serial_${EXAMPLE}_${KEY_AGRIF}.log
  date
  echo TEST SERIAL $EXAMPLE ${KEY_AGRIF}
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD serial_${EXAMPLE}_${KEY_AGRIF}.log
  end
echo "Remove *.exe* "
#/bin/rm *.exe*
echo "-------"
end
end
#########################################################################################
########################################################################################
# OpenMP
#########################################################################################
########################################################################################
echo 
echo define OPENMP
sed 's/'undef\ OPENMP'/'define\ OPENMP'/' < cppdefs_bak1.h > cppdefs_bak2.h
/bin/mv cppdefs_bak2.h cppdefs_bak1.h

#---------------------------------------------------------------------------------
echo "======================"
echo OPEN-MP 1X2 NPP=2 TESTS
echo
/bin/rm param_bak1.h
sed 's/'NPP=4'/'NPP=2'/' < param_bak0.h > param_bak1.h
sed 's/'NSUB_X=1,\ NSUB_E=NPP'/'NSUB_X=1,\ NSUB_E=2'/' < param_bak1.h > param_bak2.h

setenv OMP_NUM_THREADS 2
#
foreach EXAMPLE ( $LIST_EXAMPLE )
 foreach KEY_AGRIF ( $LIST_KEY_AGRIF )
  echo COMPILE OPENMP 1X2 $EXAMPLE


  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'undef\ $KEY_AGRIF'/'define\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

if ( $Nesting2W == 0 ) then
  sed s/'define AGRIF_2WAY'/'undef AGRIF_2WAY'/ < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
endif

  date
  time ./jobcomp_rvtk_omp > jobcomp_openmp_${EXAMPLE}_${KEY_AGRIF}.log
  date

  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'define\ $KEY_AGRIF'/'undef\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h


  /bin/mv roms roms_omp1X2_${EXAMPLE}_${KEY_AGRIF}.exe
  echo RUN OPENMP 1X2 $EXAMPLE _${KEY_AGRIF}
  time roms_omp1X2_${EXAMPLE}_${KEY_AGRIF}.exe > openmp1X2_${EXAMPLE}_${KEY_AGRIF}.log
  date


  echo TEST OPENMP 1X2  $EXAMPLE _ ${KEY_AGRIF}
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp1X2_${EXAMPLE}_${KEY_AGRIF}.log
  end
echo "Remove *.exe* "
/bin/rm *.exe*
echo "-------"
end
end
#########################################################################################
echo
echo OPEN-MP 2X1 NPP=2 TESTS
echo
/bin/rm param_bak1.h
sed 's/'NPP=4'/'NPP=2'/' < param_bak0.h > param_bak1.h
sed 's/'NSUB_X=1,\ NSUB_E=NPP'/'NSUB_X=2,\ NSUB_E=1'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h

setenv OMP_NUM_THREADS 2


foreach EXAMPLE ( $LIST_EXAMPLE )
 foreach KEY_AGRIF ( $LIST_KEY_AGRIF )

  echo COMPILE OPENMP 2X1 $EXAMPLE - $KEY_AGRIF
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'undef\ $KEY_AGRIF'/'define\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

if ( $Nesting2W == 0 ) then
  sed s/'define AGRIF_2WAY'/'undef AGRIF_2WAY'/ < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
endif

  date
  time ./jobcomp_rvtk_omp > jobcomp_openmp_${EXAMPLE}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'define\ $KEY_AGRIF'/'undef\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

  /bin/mv roms roms_omp2X1_${EXAMPLE}_${KEY_AGRIF}.exe
  echo RUN OPENMP 2X1 $EXAMPLE _$KEY_AGRIF

  time roms_omp2X1_${EXAMPLE}_${KEY_AGRIF}.exe > openmp2X1_${EXAMPLE}_${KEY_AGRIF}.log
  date

  echo TEST OPENMP 2X1 $EXAMPLE _$KEY_AGRIF
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp2X1_${EXAMPLE}_${KEY_AGRIF}.log
  end
echo "Remove *.exe* "
/bin/rm *.exe*
echo "-------"
end
end
#########################################################################################
echo
echo OPEN-MP 2X2 NPP=2 TESTS
echo
/bin/rm param_bak1.h
sed 's/'NPP=4'/'NPP=2'/' < param_bak0.h > param_bak1.h
sed 's/'NSUB_X=1,\ NSUB_E=NPP'/'NSUB_X=2,\ NSUB_E=2'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
setenv OMP_NUM_THREADS 2
#
foreach EXAMPLE ( $LIST_EXAMPLE )
 foreach KEY_AGRIF ( $LIST_KEY_AGRIF )
  echo COMPILE OPENMP 4X4 $EXAMPLE 
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'undef\ $KEY_AGRIF'/'define\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

if ( $Nesting2W == 0 ) then
  sed s/'define AGRIF_2WAY'/'undef AGRIF_2WAY'/ < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
endif

  date
  time jobcomp_rvtk_omp > jobcomp_openmp_${EXAMPLE}_${KEY_AGRIF}.log
  date

  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'define\ $KEY_AGRIF'/'undef\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

  /bin/mv roms roms_omp4X4_${EXAMPLE}_${KEY_AGRIF}.exe
  echo RUN OPENMP 4X4 $EXAMPLE _${KEY_AGRIF}
  time roms_omp4X4_${EXAMPLE}_${KEY_AGRIF}.exe > openmp4X4_${EXAMPLE}_${KEY_AGRIF}.log
  date
  echo TEST OPENMP 4X4 $EXAMPLE _${KEY_AGRIF} 
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp4X4_${EXAMPLE}_${KEY_AGRIF}.log
  end
echo "Remove *.exe* "
/bin/rm *.exe*
echo "-------"
end
end
#########################################################################################
echo
echo OPEN-M 1X4 NPP=4 TESTS
echo
/bin/rm param_bak1.h
sed 's/'NPP=4'/'NPP=4'/' < param_bak0.h > param_bak1.h
sed 's/'NSUB_X=1,\ NSUB_E=NPP'/'NSUB_X=1,\ NSUB_E=4'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
setenv OMP_NUM_THREADS 4
#
foreach EXAMPLE ( $LIST_EXAMPLE )
 foreach KEY_AGRIF ( $LIST_KEY_AGRIF )
  echo COMPILE OPENMP 1X4 $EXAMPLE


  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'undef\ $KEY_AGRIF'/'define\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

if ( $Nesting2W == 0 ) then
  sed s/'define AGRIF_2WAY'/'undef AGRIF_2WAY'/ < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
endif

  date
  time ./jobcomp_rvtk_omp > jobcomp_openmp_${EXAMPLE}_${KEY_AGRIF}.log
  date

  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'define\ $KEY_AGRIF'/'undef\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h


  /bin/mv roms roms_omp1X4_${EXAMPLE}_${KEY_AGRIF}.exe
  echo RUN OPENMP 1X4 $EXAMPLE _${KEY_AGRIF}
  time roms_omp1X4_${EXAMPLE}_${KEY_AGRIF}.exe > openmp1X4_${EXAMPLE}_${KEY_AGRIF}.log
  date


  echo TEST OPENMP 1X4  $EXAMPLE _ ${KEY_AGRIF}
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp1X4_${EXAMPLE}_${KEY_AGRIF}.log
  end
echo "Remove *.exe* "
/bin/rm *.exe*
echo "-------"
end
end
#########################################################################################
echo
echo OPEN-M 4X1 NPP=4 TESTS
echo
/bin/rm param_bak1.h
sed 's/'NPP=4'/'NPP=4'/' < param_bak0.h > param_bak1.h
sed 's/'NSUB_X=1,\ NSUB_E=NPP'/'NSUB_X=4,\ NSUB_E=1'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
setenv OMP_NUM_THREADS 4
#
foreach EXAMPLE ( $LIST_EXAMPLE )
 foreach KEY_AGRIF ( $LIST_KEY_AGRIF )
  echo COMPILE OPENMP 4X1 $EXAMPLE


  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'undef\ $KEY_AGRIF'/'define\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

if ( $Nesting2W == 0 ) then
  sed s/'define AGRIF_2WAY'/'undef AGRIF_2WAY'/ < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
endif

  date
  time ./jobcomp_rvtk_omp > jobcomp_openmp_${EXAMPLE}_${KEY_AGRIF}.log
  date

  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'define\ $KEY_AGRIF'/'undef\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h


  /bin/mv roms roms_omp4X1_${EXAMPLE}_${KEY_AGRIF}.exe
  echo RUN OPENMP 4X1 $EXAMPLE _${KEY_AGRIF}
  time roms_omp4X1_${EXAMPLE}_${KEY_AGRIF}.exe > openmp4X1_${EXAMPLE}_${KEY_AGRIF}.log
  date


  echo TEST OPENMP 4X1  $EXAMPLE _ ${KEY_AGRIF}
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp4X1_${EXAMPLE}_${KEY_AGRIF}.log
  end
echo "Remove *.exe* "
/bin/rm *.exe*
echo "-------"
end
end
#########################################################################################
echo
echo OPEN-M 2X4 NPP=4 TESTS
echo
/bin/rm param_bak1.h
sed 's/'NPP=4'/'NPP=8'/' < param_bak0.h > param_bak1.h
sed 's/'NSUB_X=1,\ NSUB_E=NPP'/'NSUB_X=2,\ NSUB_E=4'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
setenv OMP_NUM_THREADS 8
#
foreach EXAMPLE ( $LIST_EXAMPLE )
 foreach KEY_AGRIF ( $LIST_KEY_AGRIF )
  echo COMPILE OPENMP 2X4 $EXAMPLE


  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'undef\ $KEY_AGRIF'/'define\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

if ( $Nesting2W == 0 ) then
  sed s/'define AGRIF_2WAY'/'undef AGRIF_2WAY'/ < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
endif

  date
  time ./jobcomp_rvtk_omp > jobcomp_openmp_${EXAMPLE}_${KEY_AGRIF}.log
  date

  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'define\ $KEY_AGRIF'/'undef\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h


  /bin/mv roms roms_omp2X4_${EXAMPLE}_${KEY_AGRIF}.exe
  echo RUN OPENMP 2X4 $EXAMPLE _${KEY_AGRIF}
  time roms_omp2X4_${EXAMPLE}_${KEY_AGRIF}.exe > openmp2X4_${EXAMPLE}_${KEY_AGRIF}.log
  date


  echo TEST OPENMP 2X4  $EXAMPLE _ ${KEY_AGRIF}
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp2X4_${EXAMPLE}_${KEY_AGRIF}.log
  end
echo "Remove *.exe* "
/bin/rm *.exe*
echo "-------"
end
end

#########################################################################################
echo
echo OPEN-M 4X2 NPP=4 TESTS
echo

/bin/rm param_bak1.h
sed 's/'NPP=4'/'NPP=8'/' < param_bak0.h > param_bak1.h
sed 's/'NSUB_X=1,\ NSUB_E=NPP'/'NSUB_X=4,\ NSUB_E=2'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
setenv OMP_NUM_THREADS 8
#
foreach EXAMPLE ( $LIST_EXAMPLE )
 foreach KEY_AGRIF ( $LIST_KEY_AGRIF )
  echo COMPILE OPENMP 4X2 $EXAMPLE


  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'undef\ $KEY_AGRIF'/'define\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

if ( $Nesting2W == 0 ) then
  sed s/'define AGRIF_2WAY'/'undef AGRIF_2WAY'/ < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
endif

  date
  time ./jobcomp_rvtk_omp > jobcomp_openmp_${EXAMPLE}_${KEY_AGRIF}.log
  date

  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'define\ $KEY_AGRIF'/'undef\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h


  /bin/mv roms roms_omp4X2_${EXAMPLE}_${KEY_AGRIF}.exe
  echo RUN OPENMP 4X2 $EXAMPLE _${KEY_AGRIF}
  time roms_omp4X2_${EXAMPLE}_${KEY_AGRIF}.exe > openmp4X2_${EXAMPLE}_${KEY_AGRIF}.log
  date


  echo TEST OPENMP 4X2  $EXAMPLE _ ${KEY_AGRIF}
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp4X2_${EXAMPLE}_${KEY_AGRIF}.log
  end
echo "Remove *.exe* "
/bin/rm *.exe*
echo "-------"
end
end
#########################################################################################
echo
echo OPEN-M 1X8 NPP=8 TESTS
echo
/bin/rm param_bak1.h
sed 's/'NPP=4'/'NPP=8'/' < param_bak0.h > param_bak1.h
sed 's/'NSUB_X=1,\ NSUB_E=NPP'/'NSUB_X=1,\ NSUB_E=8'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
setenv OMP_NUM_THREADS 8
#
foreach EXAMPLE ( $LIST_EXAMPLE )
 foreach KEY_AGRIF ( $LIST_KEY_AGRIF )
  echo COMPILE OPENMP 1X8 $EXAMPLE


  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'undef\ $KEY_AGRIF'/'define\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

if ( $Nesting2W == 0 ) then
  sed s/'define AGRIF_2WAY'/'undef AGRIF_2WAY'/ < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
endif

  date
  time ./jobcomp_rvtk_omp > jobcomp_openmp_${EXAMPLE}_${KEY_AGRIF}.log
  date

  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'define\ $KEY_AGRIF'/'undef\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h


  /bin/mv roms roms_omp1X8_${EXAMPLE}_${KEY_AGRIF}.exe
  echo RUN OPENMP 1X8 $EXAMPLE _${KEY_AGRIF}
  time roms_omp1X8_${EXAMPLE}_${KEY_AGRIF}.exe > openmp1X8_${EXAMPLE}_${KEY_AGRIF}.log
  date


  echo TEST OPENMP 1X8  $EXAMPLE _ ${KEY_AGRIF}
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp1X8_${EXAMPLE}_${KEY_AGRIF}.log
  end
echo "Remove *.exe* "
/bin/rm *.exe*
echo "-------"
end
end
#########################################################################################
echo
echo OPEN-M 8X1 NPP=8 TESTS
echo
/bin/rm param_bak1.h
sed 's/'NPP=4'/'NPP=8'/' < param_bak0.h > param_bak1.h
sed 's/'NSUB_X=1,\ NSUB_E=NPP'/'NSUB_X=8,\ NSUB_E=1'/' < param_bak1.h > param_bak2.h
/bin/mv param_bak2.h param_bak1.h
setenv OMP_NUM_THREADS 8
#
foreach EXAMPLE ( $LIST_EXAMPLE )
 foreach KEY_AGRIF ( $LIST_KEY_AGRIF )
  echo COMPILE OPENMP 8X1 $EXAMPLE


  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'undef\ $KEY_AGRIF'/'define\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

if ( $Nesting2W == 0 ) then
  sed s/'define AGRIF_2WAY'/'undef AGRIF_2WAY'/ < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
endif

  date
  time ./jobcomp_rvtk_omp > jobcomp_openmp_${EXAMPLE}_${KEY_AGRIF}.log
  date

  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'define\ $KEY_AGRIF'/'undef\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h


  /bin/mv roms roms_omp8X1_${EXAMPLE}_${KEY_AGRIF}.exe
  echo RUN OPENMP 8X1 $EXAMPLE _${KEY_AGRIF}
  time roms_omp8X1_${EXAMPLE}_${KEY_AGRIF}.exe > openmp8X1_${EXAMPLE}_${KEY_AGRIF}.log
  date


  echo TEST OPENMP 8X1  $EXAMPLE _ ${KEY_AGRIF}
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD openmp8X1_${EXAMPLE}_${KEY_AGRIF}.log
  end
echo "Remove *.exe* "
/bin/rm *.exe*
echo "-------"
end
end
#########################################################################################

# Cleaning
/bin/rm cppdefs_bak1.h param_bak0.h param_bak1.h



