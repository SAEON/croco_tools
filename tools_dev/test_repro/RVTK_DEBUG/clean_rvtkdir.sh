#!/bin/bash
echo '============================================================='
echo 'Clean RVTK_DIR '
echo '  '
rm -Rf ROMS_FILES VHR RIP VORTEX JET SHOREFACE TEST_CASES Compile Scripts
rm -Rf roms.in* rvtk_* jobcomp_rvtk.bash *.h Recap* *.exe *.log *.pbs AGRIF_FixedGrids.in*
rm -Rf check_file  kRGB61.txt svninfos gitinfos namelist.trc.sms
rm -Rf Log *.slurm test_croco.sh test_roms.sh gitinfo.sh  extract_results_* 

echo 'Well done: Cleaning achieved'
echo '============================================================='
