#!/bin/bash
#SBATCH --job-name=Comp_wps_real
#SBATCH --ntasks=8
#SBATCH --ntasks-per-node=40
#SBATCH --time=12:00:00              # temps d execution maximum demande (HH:MM:SS)
#SBATCH --output=comp_wps.out
#SBATCH --error=comp_wps.err
#SBATCH -A khe@cpu

cd ${SLURM_SUBMIT_DIR}

#===============================================================================

umask 022
set -u
set +x   #  set -x par defaut sur Curie

#./run_wps.bash configure.namelist.wps_BENGUELA 16
./run_real.bash configure.namelist.wps_BENGUELA 8

