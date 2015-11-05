#!/bin/bash

#======================================================================
# Global declaration
#======================================================================

do_svnupdate=off

echo
echo '============================================== '
echo
echo '         TEST ROMS WITH DEBUG RVTK            '
echo
echo '============================================== '

#
# TEST = TESTCASES, REGIONAL or VORTEX
#
TEST=$1
echo
if [ $# -eq 0 ]; then
    echo "No configuration argument supplied: take TESTCASES"
    TEST=TESTCASES
else
    echo "Configuration argument supplied: $TEST"
fi

#
# Get code source from jobcomp file
#
sed -n -e '/SOURCE=/p' jobcomp_rvtk.bash > tmp1
sed -n '$p' tmp1 > tmp2
eval "SOURCE_ROMS=`sed -n -e '/SOURCE=/ s/.*\= *//p' tmp2`"
rm -f tmp1 tmp2
SOURCE_ROMS=$HOME/GIT/croco/Roms_Agrif
echo
echo 'SOURCE_ROMS='$SOURCE_ROMS

#
# Get compilator
#
OS=`uname`
echo
echo "OPERATING SYSTEM IS: $OS"
if [[ $OS == Linux ]] ; then
        export compilo=`sed -n -e '/LINUX_FC=/ s/.*\= *//p' jobcomp_rvtk.bash`
elif [[ $OS == Darwin ]] ; then
        export compilo=`sed -n -e '/DARWIN_FC=/ s/.*\= *//p' jobcomp_rvtk.bash`
fi

#
# MAKE CONSISTENT CHOICES
#
if [[ $compilo == ifort ]]; then
 echo ' '
 echo "Compilator is: "$compilo
 echo
 export PATH=/usr/local/bin:/opt/intel/Compiler/11.1/058/bin:$PATH
 export LD_LIBRARY_PATH=/usr/local/lib
else
 echo
 echo " Compilator is: "$compilo
 echo
 export PATH=/usr/local/bin:/opt/local/bin:/opt/local/sbin:$PATH
 export LD_LIBRARY_PATH=/usr/local/lib
fi

export MPIRUN=`which mpirun`
export RVTK_DIR=`pwd`
echo 'TESTING processed in RVTK_DIR='$RVTK_DIR
echo

############################################################################
#  Update the source
#-------------------
echo
echo 'PATH='$PATH
echo
echo 'LD_LIBRARY_PATH='$LD_LIBRARY_PATH
echo
echo 'MPIRUN='$MPIRUN

# SVN update of the code
#-----------------------
if [[ $do_svnupdate == on ]]; then
    echo
    echo "PROCESS SVN UPDATE"
    svn update $SOURCE_ROMS/..
    echo "SVN UPDATE DONE"
else
    echo
    echo "NO SVN UPDATE"
fi
# Get the revision of the sources
#--------------------------------
svn info $SOURCE_ROMS/.. > svninfos
numrev0=`grep "vision" svninfos | head -n 1 | cut -f2 -d:`
numrev=`echo $numrev0`
numrev=`echo $numrev0 | tr -d [:blank:]`
echo
echo Rev$numrev

# Run RVTK
#----------
echo "Run rvtk"
cd $RVTK_DIR
echo "Remove all Recap log files "
/bin/rm -f Recap_${TEST}*
echo 
echo "Run rvtk_${TEST}.csh"
./rvtk_${TEST}.csh > Recap_${TEST} 2>&1

# Clean output and locate BUGBIN
#--------------------------------
today=`date +%Y%m%d`
mv Recap_${TEST} Recap_${TEST}_${today}
echo "extract_results_roms.pl"
./extract_results_roms.pl "$TEST"
mv Results_${TEST}_${today} ./Log

mv *.log ./Log

# Copy the file on the legos network
#-----------------------------------
#scp "./Log/Results_${TEST}_${today}" cambon@torres.legos.obs-mip.fr:/home/olvac/cambon/Rvtk_Debug_CR

cd $RVTK_DIR



