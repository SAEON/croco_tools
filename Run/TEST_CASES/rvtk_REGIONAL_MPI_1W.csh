#!/bin/csh
#
#
unalias rm
unalias mv
unalias cp
#
# Lists
#
set LIST_EXAMPLE='REGIONAL'
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
set   Nesting2W=0
echo '2WAYS NESTING FLAG = ' $Nesting2W
if ( $Nesting2W == 0 ) then
   echo 'AGRIF 1 Ways'
else
   echo 'AGRIF 2 Ways'
endif
#########################################################################################
########################################################################################
#
# Serial runs
#########################################################################################
########################################################################################
echo 
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
/bin/rm *.exe*
echo "-------"
end
end
#########################################################################################
#
# MPI
#
#########################################################################################
echo '------------------------------'
echo '---MPI TESTING MPI TESTING----'
echo '------------------------------'
#########################################################################################
echo 
echo MPI 1X2 TESTS
sed 's/'undef\ MPI'/'define\ MPI'/' < cppdefs_bak1.h > cppdefs_bak2.h
/bin/mv cppdefs_bak2.h cppdefs_bak1.h


/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=1,\ NP_ETA=2'/' < param_bak0.h > param_bak1.h
#
foreach EXAMPLE ( $LIST_EXAMPLE )
 foreach KEY_AGRIF ( $LIST_KEY_AGRIF )
  echo COMPILE MPI 1X2 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'undef\ $KEY_AGRIF'/'define\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

if ( $Nesting2W == 0 ) then
   echo 'AGRIF 1 Ways'
  sed s/'define AGRIF_2WAY'/'undef AGRIF_2WAY'/ < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
endif

 date
  time ./jobcomp_rvtk_mpi > jobcomp_mpi_${EXAMPLE}_${KEY_AGRIF}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'define\ $KEY_AGRIF'/'undef\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h



  /bin/mv roms roms_mpi1X2_${EXAMPLE}_${KEY_AGRIF}.exe
  echo RUN MPI 1X2 $EXAMPLE
  time mpirun -np 2 roms_mpi1X2_${EXAMPLE}_${KEY_AGRIF}.exe > mpi1X2_${EXAMPLE}_${KEY_AGRIF}.log
  date
  echo TEST MPI 1X2 $EXAMPLE
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi1X2_${EXAMPLE}_${KEY_AGRIF}.log
  end

end
end


#########################################################################################
echo MPI 2X1 TESTS
/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=2,\ NP_ETA=1'/' < param_bak0.h > param_bak1.h
#
foreach EXAMPLE ( $LIST_EXAMPLE )
 foreach KEY_AGRIF ( $LIST_KEY_AGRIF )
  echo COMPILE MPI 2X1 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'undef\ $KEY_AGRIF'/'define\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

if ( $Nesting2W == 0 ) then
  sed s/'define AGRIF_2WAY'/'undef AGRIF_2WAY'/ < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
endif

 date
  time ./jobcomp_rvtk_mpi > jobcomp_mpi_${EXAMPLE}_${KEY_AGRIF}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'define\ $KEY_AGRIF'/'undef\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h
  /bin/mv roms roms_mpi2X1_${EXAMPLE}_${KEY_AGRIF}.exe
  echo RUN MPI 2X1 $EXAMPLE
  time mpirun -np 2 roms_mpi2X1_${EXAMPLE}_${KEY_AGRIF}.exe > mpi2X1_${EXAMPLE}_${KEY_AGRIF}.log
  date
  echo TEST MPI 2X1 $EXAMPLE
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi2X1_${EXAMPLE}_${KEY_AGRIF}.log
  end
echo "Remove *.exe* "
/bin/rm *.exe*
echo "-------"
end
end


#########################################################################################
echo MPI 2X2 TESTS

/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=2,\ NP_ETA=2'/' < param_bak0.h > param_bak1.h
#
foreach EXAMPLE ( $LIST_EXAMPLE )
 foreach KEY_AGRIF ( $LIST_KEY_AGRIF )
  echo COMPILE MPI 2X2 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'undef\ $KEY_AGRIF'/'define\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

if ( $Nesting2W == 0 ) then 
   sed s/'define AGRIF_2WAY'/'undef AGRIF_2WAY'/ < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
endif

  date
  time ./jobcomp_rvtk_mpi > jobcomp_mpi_${EXAMPLE}_${KEY_AGRIF}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'define\ $KEY_AGRIF'/'undef\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

  /bin/mv roms roms_mpi2X2_${EXAMPLE}_${KEY_AGRIF}.exe
  echo RUN MPI 2X2 $EXAMPLE
  time mpirun -np 4 roms_mpi2X2_${EXAMPLE}_${KEY_AGRIF}.exe > mpi2X2_${EXAMPLE}_${KEY_AGRIF}.log
  date
  echo TEST MPI 2X2 $EXAMPLE

  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi2X2_${EXAMPLE}_${KEY_AGRIF}.log
  end
echo "Remove *.exe* "
/bin/rm *.exe*
echo "-------"
end
end

#########################################################################################
echo MPI 1X4 TESTS

/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=1,\ NP_ETA=4'/' < param_bak0.h > param_bak1.h
#
foreach EXAMPLE ( $LIST_EXAMPLE )
 foreach KEY_AGRIF ( $LIST_KEY_AGRIF )
  echo COMPILE MPI 1X4 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'undef\ $KEY_AGRIF'/'define\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

if ( $Nesting2W == 0 ) then 
   sed s/'define AGRIF_2WAY'/'undef AGRIF_2WAY'/ < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
endif

  date
  time ./jobcomp_rvtk_mpi > jobcomp_mpi_${EXAMPLE}_${KEY_AGRIF}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'define\ $KEY_AGRIF'/'undef\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

  /bin/mv roms roms_mpi1X4_${EXAMPLE}_${KEY_AGRIF}.exe
  echo RUN MPI 1X4 $EXAMPLE
  time mpirun -np 4 roms_mpi1X4_${EXAMPLE}_${KEY_AGRIF}.exe > mpi1X4_${EXAMPLE}_${KEY_AGRIF}.log
  date
  echo TEST MPI 1X4 $EXAMPLE

  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi1X4_${EXAMPLE}_${KEY_AGRIF}.log
  end
echo "Remove *.exe* "
/bin/rm *.exe*
echo "-------"
end
end

#########################################################################################
echo MPI 4X1 TESTS

/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=4,\ NP_ETA=1'/' < param_bak0.h > param_bak1.h
#
foreach EXAMPLE ( $LIST_EXAMPLE )
 foreach KEY_AGRIF ( $LIST_KEY_AGRIF )
  echo COMPILE MPI 4X1 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'undef\ $KEY_AGRIF'/'define\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

if ( $Nesting2W == 0 ) then 
   sed s/'define AGRIF_2WAY'/'undef AGRIF_2WAY'/ < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
endif

  date
  time ./jobcomp_rvtk_mpi > jobcomp_mpi_${EXAMPLE}_${KEY_AGRIF}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'define\ $KEY_AGRIF'/'undef\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

  /bin/mv roms roms_mpi4X1_${EXAMPLE}_${KEY_AGRIF}.exe
  echo RUN MPI 4X1 $EXAMPLE
  time mpirun -np 4 roms_mpi4X1_${EXAMPLE}_${KEY_AGRIF}.exe > mpi4X1_${EXAMPLE}_${KEY_AGRIF}.log
  date
  echo TEST MPI 4X1 $EXAMPLE

  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi4X1_${EXAMPLE}_${KEY_AGRIF}.log
  end
echo "Remove *.exe* "
/bin/rm *.exe*
echo "-------"
end
end

#########################################################################################
echo MPI 2X4 TESTS
/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=2,\ NP_ETA=4'/' < param_bak0.h > param_bak1.h
#

foreach EXAMPLE ( $LIST_EXAMPLE )
 foreach KEY_AGRIF ( $LIST_KEY_AGRIF )

  echo COMPILE MPI 2X4 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'undef\ $KEY_AGRIF'/'define\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

if ( $Nesting2W == 0 ) then
  sed s/'define AGRIF_2WAY'/'undef AGRIF_2WAY'/ < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
endif
 
  date
  time ./jobcomp_rvtk_mpi > jobcomp_mpi_${EXAMPLE}_${KEY_AGRIF}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'define\ $KEY_AGRIF'/'undef\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h


  /bin/mv roms roms_mpi2X4_${EXAMPLE}_${KEY_AGRIF}.exe
  echo RUN MPI 2X4 $EXAMPLE
  time mpirun -np 8 roms_mpi2X4_${EXAMPLE}_${KEY_AGRIF}.exe > mpi2X4_${EXAMPLE}_${KEY_AGRIF}.log
  date
  echo TEST MPI 2X4 $EXAMPLE

  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi2X4_${EXAMPLE}_${KEY_AGRIF}.log
  end
echo "Remove *.exe* "
/bin/rm *.exe*
echo "-------"
end
end


#########################################################################################
echo MPI 4X2 TESTS


/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=4,\ NP_ETA=2'/' < param_bak0.h > param_bak1.h
#
foreach EXAMPLE ( $LIST_EXAMPLE )
 foreach KEY_AGRIF ( $LIST_KEY_AGRIF )
  echo COMPILE MPI 4X2 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'undef\ $KEY_AGRIF'/'define\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

if ( $Nesting2W == 0 ) then
  sed s/'define AGRIF_2WAY'/'undef AGRIF_2WAY'/ < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
endif

  date
  time ./jobcomp_rvtk_mpi > jobcomp_mpi_${EXAMPLE}_${KEY_AGRIF}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'define\ $KEY_AGRIF'/'undef\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h



  /bin/mv roms roms_mpi4X2_${EXAMPLE}_${KEY_AGRIF}.exe
  echo RUN MPI 4X2 $EXAMPLE
  time mpirun -np 8 roms_mpi4X2_${EXAMPLE}_${KEY_AGRIF}.exe > mpi4X2_${EXAMPLE}_${KEY_AGRIF}.log
  date
  echo TEST MPI 4X2 $EXAMPLE

  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi4X2_${EXAMPLE}_${KEY_AGRIF}.log
  end
echo "Remove *.exe* "
/bin/rm *.exe*
echo "-------"
end
end

#########################################################################################
echo MPI 1X8 TESTS
/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h

sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=1,\ NP_ETA=8'/' < param_bak0.h > param_bak1.h
#
foreach EXAMPLE ( $LIST_EXAMPLE )
 foreach KEY_AGRIF ( $LIST_KEY_AGRIF )

  echo COMPILE MPI 1X8 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'undef\ $KEY_AGRIF'/'define\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

if ( $Nesting2W == 0 )  then
  sed s/'define AGRIF_2WAY'/'undef AGRIF_2WAY'/ < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
endif

  date
  time ./jobcomp_rvtk_mpi > jobcomp_mpi_${EXAMPLE}_${KEY_AGRIF}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'define\ $KEY_AGRIF'/'undef\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h



  /bin/mv roms roms_mpi1X8_${EXAMPLE}_${KEY_AGRIF}.exe
  echo RUN MPI 1X8 $EXAMPLE
  time mpirun -np 8 roms_mpi1X8_${EXAMPLE}_${KEY_AGRIF}.exe > mpi1X8_${EXAMPLE}_${KEY_AGRIF}.log
  date
  echo TEST MPI 1X8 $EXAMPLE

  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi1X8_${EXAMPLE}_${KEY_AGRIF}.log
  end
echo "Remove *.exe* "
/bin/rm *.exe*
echo "-------"
end
end
#########################################################################################
echo MPI 8X1 TESTS
/bin/rm param_bak1.h
/bin/cp param_bak0.h param_bak1.h


sed 's/'NP_XI=1,\ NP_ETA=4'/'NP_XI=8,\ NP_ETA=1'/' < param_bak0.h > param_bak1.h
#

foreach EXAMPLE ( $LIST_EXAMPLE )
 foreach KEY_AGRIF ( $LIST_KEY_AGRIF )

  echo COMPILE MPI 8X1 $EXAMPLE
  sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'undef\ $KEY_AGRIF'/'define\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

if ( $Nesting2W == 0 ) then 
  sed s/'define AGRIF_2WAY'/'undef AGRIF_2WAY'/ < cppdefs_bak1.h > cppdefs_bak2.h
  /bin/mv cppdefs_bak2.h cppdefs_bak1.h
endif

  date
  time ./jobcomp_rvtk_mpi > jobcomp_mpi_${EXAMPLE}_${KEY_AGRIF}.log
  date
  sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
  sed 's/'define\ $KEY_AGRIF'/'undef\ $KEY_AGRIF'/' < cppdefs_bak2.h > cppdefs_bak3.h
  /bin/mv cppdefs_bak3.h cppdefs_bak1.h

  /bin/mv roms roms_mpi8X1_${EXAMPLE}_${KEY_AGRIF}.exe
  echo RUN MPI 8X1 $EXAMPLE
  time mpirun -np 8 roms_mpi8X1_${EXAMPLE}_${KEY_AGRIF}.exe > mpi8X1_${EXAMPLE}_${KEY_AGRIF}.log
  date
  echo TEST MPI 8X1 $EXAMPLE
  foreach WORD ( $LIST_WORDS )
    grep --binary-files=text $WORD mpi8X1_${EXAMPLE}_${KEY_AGRIF}.log
  end
echo "Remove *.exe* "
/bin/rm *.exe*
echo "-------"
end
end
#########################################################################################
#########################################################################################

# Cleaning
/bin/rm cppdefs_bak1.h param_bak0.h param_bak1.h
