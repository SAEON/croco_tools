#!/bin/bash

#=========================
# SLURM DIRECTIVE PART
#SBATCH --job-name=CPL
#SBATCH --output=CPL.%j.out
#SBATCH --error=CPL.%j.err
#SBATCH --ntasks=28                      # Nombre total de processus MPI
#SBATCH --ntasks-per-node=40             # Nombre de processus MPI par noeud
#SBATCH --hint=nomultithread             # 1 processus MPI par coeur physique (pas d'hyperthreading
#SBATCH --time=01:59:00                  # temps d execution maximum demande (HH:MM:SS)
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH -A xxx@cpu
##SBATCH --qos=qos_cpu-t3
#SBATCH --qos=qos_cpu-dev               # qos_cpu-dev: 02 h maxi, 128 noeuds (de 40 procs) maxi
                                        # qos_cpu-t3: 20 h maxi, 512 noeuds (de 40 procs) maxi
#===========================
echo $PWD
# on se place dans le répertoire de soumission
cd ${SLURM_SUBMIT_DIR}
#echo ${SLURM_SUBMIT_DIR}
#
module purge
module load intel-compilers/19.0.4
module load intel-mpi
module load intel-mkl
module load netcdf/4.7.2-mpi
module load netcdf-fortran/4.5.2-mpi
module load hdf5/1.10.5-mpi
module load nco
#=========================

set -x
echo $PWD

#
#coupled owa: 28 procs // wrf=12 // ww3=12 // croco 2x2=4
./run_cpl 12 12 4
#coupled oa:16 procs // wrf=12 // ww3=0 // croco 2x2=4
#./run_cpl 12 0 4
#coupled ow:24 procs // wrf=12 // ww3=12 // croco 2x2=0
#./run_cpl 12 12 0

#
# forced
#./run_frc_croco 4
#./run_frc_ww3 12
#./run_cpl_wrf 12
