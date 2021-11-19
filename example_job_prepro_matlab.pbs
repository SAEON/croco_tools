#!/bin/bash
#PBS -q mpi_1
#PBS -l select=1:ncpus=28:mpiprocs=1:mem=115g

#--------------------------------------
### PBS walltime: max for one job: 48h
#--------------------------------------
#PBS -l walltime=01:00:00

#-----------------
### PBS job name
#----------------
#PBS -N CROCO_prepro

source /usr/share/Modules/3.2.10/init/bash

cd $PBS_O_WORKDIR
pwd
date

\rm CROCO_prepro.o*

########################  WARNING  #####################################
# Make sure you have already downloaded the data. Else, it won't work...
########################################################################

matlab -nojvm -nodesktop -nosplash< prepro_cfsr.m >& prepro_cfsr.log
matlab -nojvm -nodesktop -nosplash< prepro_soda.m >& prepro_soda.log

date
