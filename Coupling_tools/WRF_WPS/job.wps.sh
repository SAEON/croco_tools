#!/bin/bash
#MSUB -r Run_wps       # request name
#MSUB -o Run_wps.o
#MSUB -e Run_wps.e
#MSUB -j oe          #  VERIFIER que c'est OK sur Curie XXX
#MSUB -n 16        # Total number of mpi task to use
#MSUB -T 7200        
#MSUB -x
#MSUB -q skylake 
#MSUB -A yourproject
#MSUB -m scratch,store,work
#===============================================================================

#===============================================================================

umask 022
set -u

./run_wps.bash configure.namelist.wps 16

