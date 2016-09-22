#!/bin/bash
echo '============================================================='
echo 'Create the link between RVTK_DEBUG/ dir . and RVTK_DEBUG_src/'
echo '  '

dir_home=$PWD/../RVTK_DEBUG_src

mkdir TEST_CASES

echo '==============='
echo 'Process input file'
ln -sf ROMS_FILES_VHR ROMS_FILES
ln -sf ${dir_home}/VHR .
ln -sf ${dir_home}/JET .
ln -sf ${dir_home}/VORTEX .
ln -sf ${dir_home}/RIP .
ln -sf ${dir_home}/SHOREFACE .

echo '==============='
echo 'Process scripts'
ln -sf ${dir_home}/gitinfo.sh .
ln -sf ${dir_home}/test_* .
ln -sf ${dir_home}/extract_* .
ln -sf ${dir_home}/rvtk* .
ln -sf ${dir_home}/Log .
cp -Rf ${dir_home}/git_process.bash .
cp -Rf ${dir_home}/jobcomp_rvtk.bash .
echo '==============='
echo 'Process namelist files'

#--
ln -sf ${dir_home}/VHR/AGRIF_FixedGrids.in.REGIONAL.VHR AGRIF_FixedGrids.in.REGIONAL
ln -sf ${dir_home}/VORTEX/AGRIF_FixedGrids.in.VORTEX .

ln -sf VHR/roms.in.VHR.1 roms.in.1
ln -sf VHR/roms.in.VHR roms.in 
#--
cd TEST_CASES
ln -sf ${dir_home}/ANA/roms.in* .
#--
ln -sf ${dir_home}/RIP/roms.in* .
ln -sf ${dir_home}/RIP/rip*.nc .
#--
ln -sf ${dir_home}/JET/roms.in* .
ln -sf ${dir_home}/JET/jet*.nc .
#--
ln -sf ${dir_home}/VORTEX/roms.in* .
ln -sf ${dir_home}/VORTEX/vortex_*.nc* .
#--
ln -sf ${dir_home}/SHOREFACE/roms.in* .
ln -sf ${dir_home}/SHOREFACE/shoreface*.nc .

cd ../

#--
echo '==============='
echo 'Process slurm files eventually needed'
#ln -sf ${dir_home}/*.pbs .
ln -sf ${dir_home}/*.slurm .

echo '  '
echo 'Well done: Finish linking'
echo '============================================================='
